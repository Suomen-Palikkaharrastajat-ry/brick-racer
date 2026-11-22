import './main.css'
import { bootGame } from './vendor/packages/brick-frame-game/runtime/game-boot.js'
import { createAudioEngine } from './audio.js'
import { Elm } from './src/Main.elm'
import { migrateRetiredTrackBestTimes } from './leaderboard-migration.js'

// Drop best times stored for the retired courses (once, before Elm reads them).
migrateRetiredTrackBestTimes('racer')

// Tilt steering streams the raw device orientation to Elm, which does its
// own calibration (`Game.Ports.tiltOrientationReceived`).
bootGame({
  elm: Elm.Main,
  env: import.meta.env,
  storagePrefix: 'racer',
  createAudioEngine,
  playerPrefix: 'Racer ',
  tiltFormat: 'object',
})
