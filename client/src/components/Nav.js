import React from 'react';
import { Link } from 'react-router';

const NavItem = ({ to, desc }) => (
  <li className='nav-item'>
    <Link to={to} className='nav-link' activeClassName='active'>
      { desc }
    </Link>
  </li>
);

const Nav = ({ items }) => (
  <nav className='navbar navbar-fixed-top navbar-dark bg-inverse'>
    <ul className='nav navbar-nav'>
      {
        items.map((item, i) => <NavItem key={i} {...item} />)
      }
    </ul>
  </nav>
);

export default Nav;
