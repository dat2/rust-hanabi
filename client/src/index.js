import React from 'react';
import ReactDOM from 'react-dom';
import App from './components/App';
import './base.scss';

// rust socket :)
import Socket from './api/socket';
const listener = new Socket(`ws://${process.env.BIND}`);
listener.on('connect', () => {

  listener.emit('SetName', 'nick');
  listener.emit('CreateChannel', 'Nicks Channel');
  listener.emit('JoinChannel', 'Nicks Channel');

  listener.on('Error', (data) => {
    console.error(data);
  });

  listener.on('SendChannels', (data) => {
    console.log(data);
  });
  listener.emit('GetChannels');
});

ReactDOM.render(
  <App />,
  document.getElementById('root')
);
