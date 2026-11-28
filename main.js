import './main.css'
import { bootGame } from './vendor/packages/brick-frame-game/runtime/game-boot.js'
import { initGameThumbnails } from './thumbnails.js'
import { createAudioEngine } from './audio.js'
import { Elm } from './src/Main.elm'
import { tiltSteerOptions } from './steer-mode.js'

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
const { ports } = bootGame({
  elm: Elm.Main,
  env: import.meta.env,
  storagePrefix: 'racer',
  createAudioEngine,
  playerPrefix: 'Racer ',
  tiltSteer: tiltSteerOptions,
})

// Part pictures for the car editor's dock (`Pages.Build`) and pictures of
// whole bases and saved builds, from the same catalog the game loads its parts
// from. The base pictures do not wait behind the dock's part pictures
// (see thumbnails.js).
initGameThumbnails(ports, import.meta.env.VITE_LDRAW_BASE || 'https://catalog.palikkaharrastajat.fi/raw')

// The car editor's build history (`Game.BuildHistory`): saved on every change,
// handed back to Elm at start-up. Storage may be blocked (private mode, full
// quota); the game then simply has no saved builds.
const BUILD_HISTORY_KEY = 'palikkakehys.racer.buildHistory'
ports?.saveBuildHistory?.subscribe((json) => {
  try {
    window.localStorage.setItem(BUILD_HISTORY_KEY, String(json ?? ''))
  } catch {
    // Not saved; the build stays in memory.
  }
})
let savedBuildHistory = ''
try {
  savedBuildHistory = window.localStorage.getItem(BUILD_HISTORY_KEY) || ''
} catch {
  savedBuildHistory = ''
}
// After the app's first subscriptions are in place.
setTimeout(() => ports?.buildHistoryLoaded?.send(savedBuildHistory), 0)
