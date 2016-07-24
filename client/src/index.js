import React from 'react';
import ReactDOM from 'react-dom';
import App from './components/App';
import './base.scss';

// rust socket :)
import Socket from './api/socket';
const listener = new Socket('ws://localhost:3012');
listener.on('connect', () => {
  listener.on('Init', (data) => {
    console.log(data);
  });
  listener.emit('Init', { test: "hi" });
});

ReactDOM.render(
  <App />,
  document.getElementById('root')
);
