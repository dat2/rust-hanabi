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
      const { data } = JSON.parse(message.data);
      if(typeof data === 'string') {
        emitter.emit(data);
      } else {
        let [event] = Object.keys(data);
        emitter.emit(event, data[event]);
      }
    };
  }

  emit(event, messageData = undefined) {
    let data = event;
    if(messageData !== undefined) {
      data = { [event]: messageData };
    }
    this.connection.send(JSON.stringify({ data }));
  }

  on(event, listener) {
    this.emitter.on(event, listener);
  }
}

export default Socket;
