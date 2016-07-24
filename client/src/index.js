import React from 'react';
import ReactDOM from 'react-dom';
import App from './components/App';
import './base.scss';

// rust socket :)
import Socket from './api/socket';
const listener = new Socket('ws://localhost:3012');
listener.on('connect', () => {
  listener.on('hey', (data) => {
    console.log(data);
  });
  listener.emit('hey', { test: "hi" });
});

ReactDOM.render(
  <App />,
  document.getElementById('root')
);
