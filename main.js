import './main.css'
import { initBricksRuntime } from './vendor/packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { decodeHash, isBinaryHash } from './vendor/packages/brick-frame-viewport/runtime/url-hash-codec.js'
import { createAudioEngine } from './audio.js'
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

let tiltAttached = false
function attachTiltListener() {
  if (tiltAttached) return
  tiltAttached = true
  window.addEventListener('deviceorientation', (event) => {
    const gamma = typeof event.gamma === 'number' ? event.gamma : 0
    const beta = typeof event.beta === 'number' ? event.beta : 0
    ports?.tiltOrientationReceived?.send({
      available: typeof event.gamma === 'number',
      gamma,
      beta,
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
