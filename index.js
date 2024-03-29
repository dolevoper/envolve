const http = require('http');
const path = require('path');
const express = require('express');
const { urlencoded, json } = require('body-parser');
const socket = require('socket.io');
const { v4: uuid } = require('uuid');

const app = express();
const server = http.createServer(app);
const io = socket(server);
const port = process.env.PORT || 3000;

let roomIds = [];
const roomsById = {};

const createRoom = adminId => {
    const roomId = uuid().substr(-8);

    roomIds = [...roomIds, roomId];
    roomsById[roomId] = { adminId };

    return roomId;
};

const closeRoom = roomId => {
    roomIds = roomIds.filter(id => id !== roomId);

    delete roomsById[roomId];
};

const elmSocketEvent = (name, payload = null) => ['elm socket event', { name, payload }];

app.use(express.static(path.join(__dirname, 'public')));
app.use(urlencoded({ extended: true }));
app.use(json());

app.get('/api/rooms/:roomId', function (req, res) {
    const { roomId } = req.params;

    if (!roomIds.includes(roomId)) {
        res.status(404)
    }

    res.end();
});

app.get('*', (_, res) => {
    res.sendFile(path.resolve(__dirname, 'public', 'index.html'));
});

io.use((socket, next) => {
    let { roomId } = socket.handshake.query;

    if (!roomId) {
        roomId = createRoom(socket.id);
    } else if (!roomIds.includes(roomId)) {
        return next(new Error(`room ${roomId} does not exist`));
    }

    socket.roomId = roomId;

    next();
});

io.on('connection', function (socket) {
    const { userName } = socket.handshake.query;
    const { roomId } = socket;
    const adminId = roomsById[roomId].adminId;
    const isAdmin = adminId === socket.id;

    socket.join(roomId);

    if (isAdmin) {
        socket.emit(...elmSocketEvent('managing', roomId));
        console.log(`${userName} created room ${roomId}`);
    } else {
        socket.to(adminId).emit(...elmSocketEvent('new user', userName));
        console.log(`${userName} joined room ${roomId}`);
    }

    socket.on('disconnect', function () {
        console.log(`${userName} disconnected`);

        if (!isAdmin) {
            io.to(adminId).emit(...elmSocketEvent('user left', userName));
        } else {
            closeRoom(roomId);

            io.in(roomId).clients(function (error, clientIds) {
                if (error) throw error;

                clientIds.forEach(clientId => io.sockets.sockets[clientId].disconnect(true));
            });
        }
    });

    socket.use((packet) => {
        if (isAdmin) {
            socket.to(roomId).emit(...packet);
        } else {
            socket.to(adminId).emit(...packet);
        }
    });
});

server.listen(port, function () {
    console.log(`server listening on *:${port}`);
});