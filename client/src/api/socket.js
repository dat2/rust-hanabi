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
      const { payload } = JSON.parse(message.data);
      if(typeof payload === 'string') {
        emitter.emit(payload);
      } else {
        let [event] = Object.keys(payload);
        emitter.emit(event, payload[event]);
      }
    };
  }

  emit(event, messageData = undefined) {
    let payload = event;
    if(messageData !== undefined) {
      payload = { [event]: messageData };
    }
    this.connection.send(JSON.stringify({ payload }));
  }

  on(event, listener) {
    this.emitter.on(event, listener);
  }
}

export default Socket;
