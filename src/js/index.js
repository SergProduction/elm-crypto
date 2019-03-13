import cookie from 'js-cookie'


import '../styled/index.styl'


const ukey = cookie.get('userkey')

console.log({ ukey })

const app = window.Elm.Main.init({
  node: document.getElementById('elm'),
  flags: ukey || ""
});

const endpoint = 'wss://app.coindaq.net/ws' // 'ws://142.93.47.26:1023' // 'wss://coindaq.net:8080'

let counterMessageWs = 0

let historyCounterMessageWs = []

let logStatMidleWsMessageTimerId = null

const logStatMidleWsMessageStart = () => (logStatMidleWsMessageTimerId = setInterval(() => {
  historyCounterMessageWs.push(counterMessageWs)
  counterMessageWs = 0

  // console.log(historyCounterMessageWs.reduce((p, n) => p + n) / historyCounterMessageWs.length)
}, 1000))


const logStatMidleWsMessageStop = () => clearInterval(logStatMidleWsMessageTimerId)


let socketAuth;
let socketDefault;


const wsListen = (ws, cb) => {
  ws.onmessage = function (event) {
    cb(event.data)
  }
  ws.onopen = function (event) {
    console.log("Web Socket default opened!");
  };
  ws.onclose = function (event) {
    console.log("Web Socket default closed.");
  };

  const r = {
    on: (event, cb) => (ws[`on${event}`] = cb, r)
  }

  return r
}


const scalableGraphData = (data/*:: {[string]: string}*/) => {
  const n1 = Object.entries(data).map(([k, v]) => ({x: k, y: v}))

  // ----

  const dates = n1.map(d => new Date("1971 00:" + d.x).valueOf())

  const minDate = Math.min(...dates)
  
  const datesToLeadMinimal = dates.map(x => x - minDate)

  const maxDate = Math.max(...datesToLeadMinimal)
  
  const datesResult = datesToLeadMinimal.map(x => x / maxDate)

  // ----

  const values = n1.map(d => parseFloat(d.y))

  const minValue = Math.min(...values)
  
  const valuesToLeadMinimal = values.map(x => x - minValue)

  const maxValue = Math.max(...valuesToLeadMinimal)

  const valuesResult = valuesToLeadMinimal.map(y => y / maxValue)

  // ----

  return valuesResult
    .map((y, i) => ({x: datesResult[i], y }))
    .sort((a, b) => a.x - b.x)
}


app.ports.saveSession.subscribe((data) => {
  console.log('saveSession', data)
  cookie.set('userkey', data, { expires: 7 }); 
})


app.ports.leaveUser.subscribe(() => {
  console.log('leaveUser')
  cookie.remove('userkey'); 
})


// ----- user ws -----
app.ports.toJs.subscribe((data) => {

  if (socketAuth === undefined) {
    
    socketAuth = new WebSocket(endpoint);

    wsListen(socketAuth, (wsData) => {
      
      if (JSON.parse(wsData).message !== undefined) {
        app.ports.wsListenUnsubcribePairs.send(wsData)
      } else {
        const d = JSON.parse(wsData)

        d.Graphics = scalableGraphData(d.Graphics)
      
        app.ports.wsListenPairs.send(JSON.stringify(d))
      }
    })
    .on('open', () => {
      socketAuth.send(JSON.stringify(data))
    })

  }

  if (socketAuth.readyState === 1) {
    socketAuth.send(JSON.stringify(data))
  }
})


// ----- default ws -----
socketDefault = new WebSocket(`${endpoint}/defaults`);

wsListen(socketDefault, (wsData) => {
  counterMessageWs++

  const d = JSON.parse(wsData)

  d.Graphics = scalableGraphData(d.Graphics)

  app.ports.wsListenPairs.send(JSON.stringify(d))
})
.on('open', () => {
  logStatMidleWsMessageStart()
  app.ports.wsDefaultStatus.send('Connect')
})
.on('close', () => {
  logStatMidleWsMessageStop()
  app.ports.wsDefaultStatus.send('Close')
})
.on('error', () => {
  logStatMidleWsMessageStop()
  app.ports.wsDefaultStatus.send('Error')
})