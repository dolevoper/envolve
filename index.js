const http = require('http');
const path = require('path');
const express = require('express');
const socket = require('socket.io');
const { v4: uuid } = require('uuid');

const app = express();
const server = http.createServer(app);
const io = socket(server);
const port = process.env.PORT || 3000;

let rooms = [];

app.use(express.static(`${__dirname}/public`));

app.get('/:roomId', function (req, res) {
    const { roomId } = req.params;

    if (!rooms.includes(roomId)) {
        return res.send(`Room ${roomId} does not exists.`);
    }

    res.sendFile(path.resolve(__dirname, 'public', 'index.html'));
});

io.on('connection', function (socket) {
    let { userName, roomId } = socket.handshake.query;

    if (!roomId) {
        roomId = socket.handshake.query.roomId || uuid().substr(-8);

        rooms = [...rooms, roomId];

        socket.emit('managing', { roomId });
    }

    console.log(`${userName} joined room ${roomId}`);

    socket.join(roomId);
    socket.to(roomId).emit('new user', { userName })

    socket.on('disconnect', function () {
        console.log(`${userName} disconnected`);

        io.in(roomId).clients(function (error, clients) {
            if (error) throw error;

            if (!clients.length) {
                console.log(`closed room ${roomId}`);
                
                rooms = rooms.filter(id => id !== roomId);
            }
        });
    });
});

server.listen(port, function () {
    console.log(`server listening on *:${port}`);
});