/**
 * Procedural Web Audio synthesizer for Brick Racer.
 * Extends BaseAudioEngine with wind bandpass noise, ground-coupled
 * wheel rumble & axle clatter, chassis scrape rasp, impact thuds, and chimes.
 */

import { BaseAudioEngine } from './vendor/packages/brick-frame-game/runtime/audio-engine.js'

export function createAudioEngine(initialMuted = false) {
  const engine = new BaseAudioEngine(initialMuted)

  let windGainNode = null
  let windFilter = null
  let rumbleGainNode = null
  let rumbleFilter = null
  let clatterGainNode = null
  let clatterOsc = null
  let scrapeGainNode = null
  let scrapeOsc = null

  engine.initContinuousLoops = function (ctx, noiseBuffer, masterGain) {
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
  }

  engine.updateContinuous = function (state) {
    if (!engine.unlocked || !engine.ctx || !state) return
    const now = engine.ctx.currentTime
    const active = Boolean(state.active) && !engine.muted
    const targetWind = active ? Math.max(0, Math.min(0.6, state.windGain || 0)) : 0
    const targetRumble = active ? Math.max(0, Math.min(0.6, state.rumbleGain || 0)) : 0
    const targetClatter = active ? Math.max(0, Math.min(0.25, state.clatterGain || 0)) : 0
    const targetScrape = active ? Math.max(0, Math.min(0.18, state.scrapeGain || 0)) : 0

    windGainNode?.gain.setTargetAtTime(targetWind, now, 0.05)
    windFilter?.frequency.setTargetAtTime(Math.max(120, Math.min(2600, state.windHz || 300)), now, 0.06)

    rumbleGainNode?.gain.setTargetAtTime(targetRumble, now, 0.04)
    rumbleFilter?.frequency.setTargetAtTime(Math.max(60, Math.min(600, state.rumbleHz || 120)), now, 0.05)

    clatterGainNode?.gain.setTargetAtTime(targetClatter, now, 0.04)
    clatterOsc?.frequency.setTargetAtTime(Math.max(18, Math.min(180, (state.rumbleHz || 80) * 0.45)), now, 0.05)

    scrapeGainNode?.gain.setTargetAtTime(targetScrape, now, 0.03)
  }

  engine.playSfx = function (event) {
    if (!event || engine.muted) return
    const intensity = Math.max(0.1, Math.min(1.0, Number(event.intensity) || 0.5))

    switch (event.kind) {
      case 'countdown':
        engine.playTone(440, 0.18, 0, 'sine', 0.22)
        break
      case 'go':
        engine.playTone(880, 0.32, 0, 'triangle', 0.28)
        break
      case 'checkpoint':
        engine.playTone(587.33, 0.14, 0, 'triangle', 0.2)
        engine.playTone(880, 0.22, 0.1, 'triangle', 0.22)
        break
      case 'finish':
        engine.playTone(523.25, 0.16, 0, 'triangle', 0.22)
        engine.playTone(659.25, 0.16, 0.11, 'triangle', 0.22)
        engine.playTone(783.99, 0.18, 0.22, 'triangle', 0.24)
        engine.playTone(1046.5, 0.45, 0.34, 'triangle', 0.28)
        break
      case 'landing':
        engine.playTone(90 + intensity * 30, 0.16, 0, 'sine', 0.32 * intensity)
        break
      case 'pop':
        engine.playTone(950, 0.05, 0, 'square', 0.14)
        engine.playTone(620, 0.07, 0.03, 'triangle', 0.16)
        break
      case 'impact':
        engine.playTone(110, 0.18, 0, 'sawtooth', 0.26 * intensity)
        engine.playTone(740, 0.06, 0.01, 'triangle', 0.18 * intensity)
        engine.playTone(510, 0.07, 0.04, 'triangle', 0.14 * intensity)
        break
      default:
        break
    }
  }

  return engine
}
