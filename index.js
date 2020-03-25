const http = require('http');
const path = require('path');
const express = require('express');
const socket = require('socket.io');
const { v4: uuid } = require('uuid');

const app = express();
const server = http.createServer(app);
const io = socket(server);
const port = process.env.PORT || 3000;

app.use(express.static(`${__dirname}/public`));

app.get('*', function (_, res) {
    res.sendFile(path.resolve(__dirname, 'public', 'index.html'));
});

io.on('connection', function (socket) {
    const userName = socket.handshake.query.userName;
    const roomId = socket.handshake.query.roomId || uuid().substr(-8);

    console.log(`${userName} joined room ${roomId}`);

    socket.join(roomId);
    socket.to(roomId).emit('new user', { userName })

    socket.on('disconnect', function () {
        console.log(`${userName} disconnected`);
    });
});

server.listen(port, function () {
    console.log(`server listening on *:${port}`);
});