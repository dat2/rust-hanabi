import React from 'react';
import styles from './App.scss';

import Nav from './Nav';

const App = ({ children, name, onNameChange }) => (
  <div>
    <Nav items={[{ to: 'game', desc: 'Game' }]} />

    <div className={'container-fluid ' + styles.children}>
      { children }
    </div>
  </div>
);

export default App;
