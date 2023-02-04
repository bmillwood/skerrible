const audioFiles = {};
var socket;
var app = Elm.Main.init({
    // flags: {}
});
app.ports.sendToJS.subscribe(function(request) {
    switch(request.kind) {
    case 'connect':
        if(socket) {
            socket.close();
        }
        socket = new WebSocket(request.payload);
        socket.addEventListener("open", function(event) {
            app.ports.receiveFromJS.send({ tag: 'server-status', contents: 'connected' });
        });
        socket.addEventListener("close", function(event) {
            app.ports.receiveFromJS.send({ tag: 'server-status', contents: 'disconnected' });
        });
        socket.addEventListener("message", function(event) {
            app.ports.receiveFromJS.send({ tag: 'from-server', contents: JSON.parse(event.data) });
        });
        break;
    case 'send':
        socket.send(JSON.stringify(request.payload));
        break;
    case 'playAudio':
        const url = request.payload;
        audioFiles[url] = audioFiles[url] || new Audio(url);
        audioFiles[url].play();
        break;
    }
});
