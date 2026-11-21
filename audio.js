/**
 * Procedural Web Audio synthesizer for Brick Racer.
 * Zero external sound assets: builds wind bandpass noise, ground-coupled
 * wheel rumble & axle clatter, chassis scrape rasp, impact thuds, and chimes.
 */
export function createAudioEngine(initialMuted = false) {
  let ctx = null
  let masterGain = null
  let windGainNode = null
  let windFilter = null
  let rumbleGainNode = null
  let rumbleFilter = null
  let clatterGainNode = null
  let clatterOsc = null
  let scrapeGainNode = null
  let scrapeOsc = null
  let muted = Boolean(initialMuted)
  let unlocked = false

  function ensureContext() {
    if (typeof window === 'undefined') return null
    const AudioCtx = window.AudioContext || window.webkitAudioContext
    if (!AudioCtx) return null
    if (ctx) {
      if (ctx.state === 'suspended') {
        ctx.resume().catch(() => {})
      }
      return ctx
    }

    ctx = new AudioCtx()

    masterGain = ctx.createGain()
    masterGain.gain.value = muted ? 0 : 0.45
    masterGain.connect(ctx.destination)

    // Shared 2-second pink-ish noise buffer
    const sampleRate = ctx.sampleRate
    const bufferLen = sampleRate * 2
    const noiseBuffer = ctx.createBuffer(1, bufferLen, sampleRate)
    const data = noiseBuffer.getChannelData(0)
    let b0 = 0
    let b1 = 0
    let b2 = 0
    for (let i = 0; i < bufferLen; i++) {
      const white = Math.random() * 2 - 1
      b0 = 0.99765 * b0 + white * 0.099046
      b1 = 0.963 * b1 + white * 0.2965164
      b2 = 0.57 * b2 + white * 1.0526913
      data[i] = Math.max(-1, Math.min(1, (b0 + b1 + b2 + white * 0.1848) * 0.16))
    }

    // 1. Wind bandpass loop
    const windSrc = ctx.createBufferSource()
    windSrc.buffer = noiseBuffer
    windSrc.loop = true
    windFilter = ctx.createBiquadFilter()
    windFilter.type = 'bandpass'
    windFilter.frequency.value = 320
    windFilter.Q.value = 0.8
    windGainNode = ctx.createGain()
    windGainNode.gain.value = 0
    windSrc.connect(windFilter)
    windFilter.connect(windGainNode)
    windGainNode.connect(masterGain)
    windSrc.start()

    // 2. Ground rumble lowpass loop
    const rumbleSrc = ctx.createBufferSource()
    rumbleSrc.buffer = noiseBuffer
    rumbleSrc.loop = true
    rumbleFilter = ctx.createBiquadFilter()
    rumbleFilter.type = 'lowpass'
    rumbleFilter.frequency.value = 120
    rumbleGainNode = ctx.createGain()
    rumbleGainNode.gain.value = 0
    rumbleSrc.connect(rumbleFilter)
    rumbleFilter.connect(rumbleGainNode)
    rumbleGainNode.connect(masterGain)
    rumbleSrc.start()

    // 3. Wheel / rim / axle clatter oscillator
    clatterOsc = ctx.createOscillator()
    clatterOsc.type = 'triangle'
    clatterOsc.frequency.value = 42
    clatterGainNode = ctx.createGain()
    clatterGainNode.gain.value = 0
    clatterOsc.connect(clatterGainNode)
    clatterGainNode.connect(masterGain)
    clatterOsc.start()

    // 4. Chassis scrape sawtooth rasp
    scrapeOsc = ctx.createOscillator()
    scrapeOsc.type = 'sawtooth'
    scrapeOsc.frequency.value = 95
    scrapeGainNode = ctx.createGain()
    scrapeGainNode.gain.value = 0
    scrapeOsc.connect(scrapeGainNode)
    scrapeGainNode.connect(masterGain)
    scrapeOsc.start()

    unlocked = true
    return ctx
  }

  function unlock() {
    ensureContext()
  }

  function setMuted(nextMuted) {
    muted = Boolean(nextMuted)
    if (ctx && masterGain) {
      masterGain.gain.setTargetAtTime(muted ? 0 : 0.45, ctx.currentTime, 0.04)
    }
  }

  function updateContinuous(state) {
    if (!unlocked || !ctx || !state) return
    const now = ctx.currentTime
    const active = Boolean(state.active) && !muted
    const targetWind = active ? Math.max(0, Math.min(0.6, state.windGain || 0)) : 0
    const targetRumble = active ? Math.max(0, Math.min(0.6, state.rumbleGain || 0)) : 0
    const targetClatter = active ? Math.max(0, Math.min(0.25, state.clatterGain || 0)) : 0
    const targetScrape = active ? Math.max(0, Math.min(0.18, state.scrapeGain || 0)) : 0

    windGainNode.gain.setTargetAtTime(targetWind, now, 0.05)
    windFilter.frequency.setTargetAtTime(Math.max(120, Math.min(2600, state.windHz || 300)), now, 0.06)

    rumbleGainNode.gain.setTargetAtTime(targetRumble, now, 0.04)
    rumbleFilter.frequency.setTargetAtTime(Math.max(60, Math.min(600, state.rumbleHz || 120)), now, 0.05)

    clatterGainNode.gain.setTargetAtTime(targetClatter, now, 0.04)
    clatterOsc.frequency.setTargetAtTime(Math.max(18, Math.min(180, (state.rumbleHz || 80) * 0.45)), now, 0.05)

    scrapeGainNode.gain.setTargetAtTime(targetScrape, now, 0.03)
  }

  function playTone(freq, duration, delay = 0, type = 'triangle', peak = 0.25) {
    const c = ensureContext()
    if (!c || muted) return
    const t = c.currentTime + delay
    const osc = c.createOscillator()
    const g = c.createGain()
    osc.type = type
    osc.frequency.setValueAtTime(freq, t)
    g.gain.setValueAtTime(0.0001, t)
    g.gain.exponentialRampToValueAtTime(peak, t + 0.015)
    g.gain.exponentialRampToValueAtTime(0.0001, t + duration)
    osc.connect(g)
    g.connect(masterGain)
    osc.start(t)
    osc.stop(t + duration + 0.02)
  }

  function playSfx(event) {
    if (!event || muted) return
    const c = ensureContext()
    if (!c) return
    const intensity = Math.max(0.1, Math.min(1.0, Number(event.intensity) || 0.5))

    switch (event.kind) {
      case 'countdown':
        playTone(440, 0.18, 0, 'sine', 0.22)
        break
      case 'go':
        playTone(880, 0.32, 0, 'triangle', 0.28)
        break
      case 'checkpoint':
        playTone(587.33, 0.14, 0, 'triangle', 0.2)
        playTone(880, 0.22, 0.1, 'triangle', 0.22)
        break
      case 'finish':
        playTone(523.25, 0.16, 0, 'triangle', 0.22)
        playTone(659.25, 0.16, 0.11, 'triangle', 0.22)
        playTone(783.99, 0.18, 0.22, 'triangle', 0.24)
        playTone(1046.5, 0.45, 0.34, 'triangle', 0.28)
        break
      case 'landing':
        playTone(90 + intensity * 30, 0.16, 0, 'sine', 0.32 * intensity)
        break
      case 'pop':
        playTone(950, 0.05, 0, 'square', 0.14)
        playTone(620, 0.07, 0.03, 'triangle', 0.16)
        break
      case 'impact':
        playTone(110, 0.18, 0, 'sawtooth', 0.26 * intensity)
        playTone(740, 0.06, 0.01, 'triangle', 0.18 * intensity)
        playTone(510, 0.07, 0.04, 'triangle', 0.14 * intensity)
        break
      default:
        break
    }
  }

  return {
    unlock,
    setMuted,
    updateContinuous,
    playSfx,
  }
}
