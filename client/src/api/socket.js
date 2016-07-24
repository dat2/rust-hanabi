import ee from 'event-emitter';

/**
 * Very similar api to socket.io, however no reliability :) and no errors lol
 */
class Socket {
  constructor(url) {
    this.emitter = ee({});
    const emitter = this.emitter;

    this.connection = new WebSocket(url);
    this.connection.onopen = function() {
      emitter.emit('connect');
    };

    this.connection.onerror = function(err) {
      emitter.emit('error', err);
    };

    this.connection.onmessage = function(message) {
      const { event, data } = JSON.parse(message.data);
      emitter.emit(event, data);
    };
  }

  emit(event, data) {
    this.connection.send(JSON.stringify({ event, data }));
  }

  on(event, listener) {
    this.emitter.on(event, listener);
  }
}

export default Socket;
