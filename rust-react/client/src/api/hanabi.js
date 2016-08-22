// rust socket :)
import socket from './socket';
socket.on('connect', () => {

  // socket.emit('SetName', 'nick');
  // socket.emit('CreateChannel', 'Nicks Channel');
  // socket.emit('JoinChannel', 'Nicks Channel');
  // socket.emit('LeaveChannel');

  socket.on('Error', (data) => {
    console.error(data);
  });

  // socket.on('SendChannels', (data) => {
  //   console.log(data);
  // });
  // socket.emit('GetChannels');
});
