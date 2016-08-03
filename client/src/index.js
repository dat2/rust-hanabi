import React from 'react';
import ReactDOM from 'react-dom';
import App from './components/App';
import './base.scss';

// rust socket :)
import Socket from './api/socket';
const listener = new Socket(`ws://${process.env.BIND}`);
listener.on('connect', () => {
  listener.on('GetState', (data) => {
    console.log(data);
  });
  listener.emit('Init');
});

ReactDOM.render(
  <App />,
  document.getElementById('root')
);
