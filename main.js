import './main.css'
import { bootGame } from './vendor/packages/brick-frame-game/runtime/game-boot.js'
import { createAudioEngine } from './audio.js'
import { Elm } from './src/Main.elm'
import { migrateRetiredTrackBestTimes } from './leaderboard-migration.js'
import { tiltSteerOptions } from './steer-mode.js'

// Drop best times stored for the retired courses (once, before Elm reads them).
migrateRetiredTrackBestTimes('racer')

// Phones play in landscape. On the first touch, ask for fullscreen and lock the
// screen to landscape where the browser allows it (fullscreen and a user gesture
// are needed on some browsers; iOS has no lock). Where the lock is not possible,
// the rotate hint in index.html and main.css covers the portrait phone.
if (window.matchMedia?.('(pointer: coarse)').matches) {
  window.addEventListener(
    'pointerdown',
    async () => {
      try {
        if (document.fullscreenEnabled && !document.fullscreenElement) {
          await document.documentElement.requestFullscreen()
        }
      } catch {
        // Fullscreen refused: the orientation lock may still work.
      }
      try {
        await screen.orientation?.lock?.('landscape')
      } catch {
        // Not supported or refused: the rotate hint takes over.
      }
    },
    { once: true, capture: true, passive: true },
  )
}

// Tilt steering is the shared calibrated `tilt-steer.js`: on by default on
// phones, the player's saved choice wins (see `steer-mode.js`).
bootGame({
  elm: Elm.Main,
  env: import.meta.env,
  storagePrefix: 'racer',
  createAudioEngine,
  playerPrefix: 'Racer ',
  tiltSteer: tiltSteerOptions,
})
