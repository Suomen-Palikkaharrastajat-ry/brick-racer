import './main.css'
import { initBricksRuntime } from './vendor/packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { decodeHash, isBinaryHash } from './vendor/packages/brick-frame-viewport/runtime/url-hash-codec.js'
import { createAudioEngine } from './audio.js'
import { createMultiplayerManager } from './vendor/packages/brick-frame-game/runtime/multiplayer.js'
import { createQrCameraScanner } from './vendor/packages/brick-frame-game/runtime/qr-camera-scanner.js'
import { Elm } from './src/Main.elm'

const ldrawBase = import.meta.env.VITE_LDRAW_BASE || 'https://catalog.palikkaharrastajat.fi/raw'
const indicesBase = import.meta.env.VITE_LDRAW_INDICES_BASE || 'https://catalog.palikkaharrastajat.fi/indices'

const LANG_KEY = 'palikkakehys.language'
const AUDIO_MUTED_KEY = 'palikkakehys.racer.audioMuted'
const LEADERBOARD_KEY = 'palikkakehys.racer.leaderboard'

function readLanguage() {
  try {
    const hashParams = new URLSearchParams(window.location.hash.replace(/^#/, ''))
    const searchParams = new URLSearchParams(window.location.search)
    const lang = searchParams.get('lang') || hashParams.get('lang')
    if (lang) {
      return lang.toLowerCase().startsWith('fi') ? 'fi' : 'en'
    }
    const stored = window.localStorage.getItem(LANG_KEY)
    if (stored) {
      return stored.toLowerCase().startsWith('fi') ? 'fi' : 'en'
    }
    return 'en'
  } catch {
    return 'en'
  }
}

function readAudioMuted() {
  try {
    return window.localStorage.getItem(AUDIO_MUTED_KEY) === '1'
  } catch {
    return false
  }
}

function readLeaderboard() {
  try {
    return window.localStorage.getItem(LEADERBOARD_KEY) || '[]'
  } catch {
    return '[]'
  }
}

function extractBinaryHashFragment(rawInput) {
  const str = String(rawInput ?? '').trim()
  const hashIdx = str.indexOf('#')
  const candidate = hashIdx >= 0 ? str.slice(hashIdx) : str

  const parts = candidate.replace(/^#/, '').split('&')
  for (const part of parts) {
    if (part.startsWith('b/')) {
      return `#${part}`
    }
  }
  if (isBinaryHash(candidate)) {
    return candidate.startsWith('#') ? candidate : `#${candidate}`
  }
  return null
}

function decodeHashToByteArray(rawHash) {
  const binaryFrag = extractBinaryHashFragment(rawHash)
  if (!binaryFrag) return null
  try {
    const bytes = decodeHash(binaryFrag)
    return bytes ? Array.from(bytes) : null
  } catch {
    return null
  }
}

const coarsePointer =
  typeof window.matchMedia === 'function' && window.matchMedia('(pointer: coarse)').matches

const initialHash = window.location.hash ?? ''
const savedAudioMuted = readAudioMuted()
const audioEngine = createAudioEngine(savedAudioMuted)

const runtime = initBricksRuntime({
  elm: Elm.Main,
  node: document.getElementById('app'),
  mode: 'viewer',
  ldrawBase,
  indicesBase,
  ldrawFallbackBase: import.meta.env.VITE_LDRAW_FALLBACK_BASE || '/raw',
  defaultModel: '',
  loadModelInJs: false,
  extraFlags: {
    coarsePointer,
    language: readLanguage(),
    savedAudioMuted,
    savedLeaderboardJson: readLeaderboard(),
    baseUrl: window.location.origin + window.location.pathname,
    predecodedInitialBytes: decodeHashToByteArray(initialHash),
  },
  initialHash,
  syncUrlHash: true,
  useWindowResize: true,
  dragDropTarget: window,
  suppressGestureTarget: window,
})

const ports = runtime.app?.ports
const multiplayer = createMultiplayerManager(ports)
const qrScanner = createQrCameraScanner(ports, {
  onScanned: (hash) => {
    const bytes = decodeHashToByteArray(hash)
    if (bytes) {
      ports?.binaryHashDecoded?.send(bytes)
      window.location.hash = hash
    }
  },
})

// Check if loaded with #room=CODE or #join=CODE
try {
  const hashStr = window.location.hash || ''
  const roomMatch = hashStr.match(/[#&](?:room|join)=([A-Za-z0-9]{4,12})/i)
  if (roomMatch && roomMatch[1]) {
    setTimeout(() => {
      multiplayer.joinRoom(roomMatch[1].toUpperCase(), 'Racer ' + Math.floor(Math.random() * 900 + 100), '')
    }, 100)
  }
} catch {
  // Ignore initial hash parse errors
}

const unlockAudioOnce = () => {
  audioEngine.unlock()
}
window.addEventListener('pointerdown', unlockAudioOnce, { passive: true })
window.addEventListener('keydown', unlockAudioOnce, { passive: true })
window.addEventListener('touchstart', unlockAudioOnce, { passive: true })

ports?.updateAudioState?.subscribe((state) => {
  audioEngine.updateContinuous(state)
})

ports?.playSfx?.subscribe((event) => {
  audioEngine.playSfx(event)
})

ports?.setAudioMuted?.subscribe((nextMuted) => {
  audioEngine.setMuted(nextMuted)
  try {
    window.localStorage.setItem(AUDIO_MUTED_KEY, nextMuted ? '1' : '0')
  } catch {
    // Ignore storage errors
  }
})

ports?.storeLanguage?.subscribe((lang) => {
  try {
    window.localStorage.setItem(LANG_KEY, lang)
  } catch {
    // Ignore storage errors
  }
})

ports?.saveLeaderboard?.subscribe((jsonStr) => {
  try {
    window.localStorage.setItem(LEADERBOARD_KEY, String(jsonStr ?? '[]'))
  } catch {
    // Ignore storage errors
  }
})

ports?.decodeBinaryHash?.subscribe((rawInput) => {
  const bytes = decodeHashToByteArray(rawInput)
  ports?.binaryHashDecoded?.send(bytes)
})

ports?.copyShareUrl?.subscribe((hashOrUrl) => {
  const full = String(hashOrUrl ?? '').startsWith('#')
    ? new URL(hashOrUrl, window.location.href).href
    : String(hashOrUrl ?? '')
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(full).catch(() => {})
  }
})

function getScreenTiltDegrees(betaDeg, gammaDeg) {
  const degToRad = Math.PI / 180
  const radToDeg = 180 / Math.PI
  const b = betaDeg * degToRad
  const g = gammaDeg * degToRad

  // Earth "Up" unit vector in physical device coordinates (W3C Z-X'-Y'' rotation matrix row 3):
  // +x = portrait right edge, +y = portrait top edge, +z = out of front screen
  const ux = -Math.cos(b) * Math.sin(g)
  const uy = Math.sin(b)
  const uz = Math.cos(b) * Math.cos(g)

  const legacyAngle = typeof window.orientation === 'number' ? window.orientation : null
  const modernAngle =
    typeof window.screen !== 'undefined' &&
    window.screen.orientation &&
    typeof window.screen.orientation.angle === 'number'
      ? window.screen.orientation.angle
      : null
  const mediaLandscape =
    typeof window.matchMedia === 'function' && window.matchMedia('(orientation: landscape)').matches

  const isLandscape =
    mediaLandscape ||
    window.innerWidth > window.innerHeight ||
    legacyAngle === 90 ||
    legacyAngle === -90 ||
    modernAngle === 90 ||
    modernAngle === 270

  let negURight
  let uTop
  if (isLandscape) {
    // Determine whether +x_dev (Landscape Left, top of phone in left hand) or
    // -x_dev (Landscape Right, top of phone in right hand) is the upper edge of the screen.
    // Using gravity ux when pitched (> ~8 deg from flat) makes this immune to iOS/Android
    // 90 vs 270 angle reporting differences.
    let topIsPosX
    if (Math.abs(ux) > 0.14) {
      topIsPosX = ux > 0
    } else if (legacyAngle === 90 || legacyAngle === -90) {
      topIsPosX = legacyAngle === 90
    } else {
      topIsPosX = modernAngle !== 270
    }

    if (topIsPosX) {
      // Landscape Left (top of phone on left): steering clockwise (right) raises +y_dev (uy > 0)
      negURight = uy
      uTop = Math.abs(ux)
    } else {
      // Landscape Right (top of phone on right): steering clockwise (right) lowers +y_dev (uy < 0)
      negURight = -uy
      uTop = Math.abs(ux)
    }
  } else if (legacyAngle === 180 || modernAngle === 180) {
    negURight = ux
    uTop = Math.abs(uy)
  } else {
    // Portrait: steering clockwise (right) lowers +x_dev (ux < 0 => -ux > 0)
    negURight = -ux
    uTop = Math.abs(uy)
  }

  const denom = Math.max(0.35, Math.hypot(uTop, uz * 0.5))
  return Math.atan2(negURight, denom) * radToDeg
}

let tiltAttached = false
function attachTiltListener() {
  if (tiltAttached) return
  tiltAttached = true
  window.addEventListener('deviceorientation', (event) => {
    const hasAngles = typeof event.gamma === 'number' || typeof event.beta === 'number'
    const rawGamma = typeof event.gamma === 'number' ? event.gamma : 0
    const rawBeta = typeof event.beta === 'number' ? event.beta : 0
    const steerGamma = hasAngles ? getScreenTiltDegrees(rawBeta, rawGamma) : 0
    ports?.tiltOrientationReceived?.send({
      available: hasAngles,
      gamma: steerGamma,
      beta: rawBeta,
    })
  })
}

ports?.requestTiltPermission?.subscribe(async () => {
  try {
    if (
      typeof DeviceOrientationEvent !== 'undefined' &&
      typeof DeviceOrientationEvent.requestPermission === 'function'
    ) {
      const result = await DeviceOrientationEvent.requestPermission()
      if (result === 'granted') {
        attachTiltListener()
      } else {
        ports?.tiltOrientationReceived?.send({ available: false, gamma: 0, beta: 0 })
      }
    } else if ('DeviceOrientationEvent' in window) {
      attachTiltListener()
    } else {
      ports?.tiltOrientationReceived?.send({ available: false, gamma: 0, beta: 0 })
    }
  } catch {
    ports?.tiltOrientationReceived?.send({ available: false, gamma: 0, beta: 0 })
  }
})

window.addEventListener('hashchange', () => {
  ports?.hashChanged?.send(window.location.hash ?? '')
})
