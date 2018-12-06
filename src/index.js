'use strict'

import './index.html'
import './style.scss'

import { Elm } from './Main.elm'

const mountNode = document.getElementById('main')
const app = Elm.Main.init({
  flags: "http://localhost:8000/sample.md"
})
