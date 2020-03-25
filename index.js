const http = require('http');
const express = require('express');
const socket = require('socket.io');

const app = express();
const server = http.createServer(app);
const io = socket(server);
const port = process.env.PORT || 3000;

app.use(express.static('./public'));

io.on('connection', function (socket) {
    const userName = socket.handshake.query.userName;

    console.log(`${userName} connected`);

    socket.broadcast.emit('new user', { userName })

    socket.on('disconnect', function () {
        console.log(`${userName} disconnected`);
    });
});

server.listen(port, function () {
    console.log(`server listening on *:${port}`);
});