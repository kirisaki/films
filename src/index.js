'use strict'

import './index.html'
import './style.scss'

import { Elm } from './Main.elm'

const mountNode = document.getElementById('main')
const app = Elm.Main.init({
  flags: "---\n\n"
    + "---\n\n"
    + "---\n\n"
})
