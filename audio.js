/**
 * Procedural Web Audio synthesizer for Brick Racer.
 * Extends BaseAudioEngine with wind bandpass noise, ground-coupled
 * wheel rumble & axle clatter, chassis scrape rasp, impact thuds, and chimes.
 */

import {
  BaseAudioEngine,
  createNoiseLoop,
  createOscillatorLoop,
} from './vendor/packages/brick-frame-game/runtime/audio-engine.js'

export function createAudioEngine(initialMuted = false) {
  const engine = new BaseAudioEngine(initialMuted)

  let wind = null
  let rumble = null
  let clatter = null
  let scrape = null

  engine.initContinuousLoops = function (ctx, noiseBuffer, masterGain) {
    // 1. Wind bandpass loop
    wind = createNoiseLoop(ctx, noiseBuffer, masterGain, { type: 'bandpass', frequency: 320, Q: 0.8 })

    // 2. Ground rumble lowpass loop
    rumble = createNoiseLoop(ctx, noiseBuffer, masterGain, { type: 'lowpass', frequency: 120 })

    // 3. Wheel / rim / axle clatter oscillator
    clatter = createOscillatorLoop(ctx, masterGain, { type: 'triangle', frequency: 42 })

    // 4. Chassis scrape sawtooth rasp
    scrape = createOscillatorLoop(ctx, masterGain, { type: 'sawtooth', frequency: 95 })
  }

  engine.updateContinuous = function (state) {
    if (!engine.unlocked || !engine.ctx || !state) return
    const now = engine.ctx.currentTime
    const active = Boolean(state.active) && !engine.muted
    const targetWind = active ? Math.max(0, Math.min(0.6, state.windGain || 0)) : 0
    const targetRumble = active ? Math.max(0, Math.min(0.6, state.rumbleGain || 0)) : 0
    const targetClatter = active ? Math.max(0, Math.min(0.25, state.clatterGain || 0)) : 0
    const targetScrape = active ? Math.max(0, Math.min(0.18, state.scrapeGain || 0)) : 0

    wind?.level.gain.setTargetAtTime(targetWind, now, 0.05)
    wind?.filter.frequency.setTargetAtTime(Math.max(120, Math.min(2600, state.windHz || 300)), now, 0.06)

    rumble?.level.gain.setTargetAtTime(targetRumble, now, 0.04)
    rumble?.filter.frequency.setTargetAtTime(Math.max(60, Math.min(600, state.rumbleHz || 120)), now, 0.05)

    clatter?.level.gain.setTargetAtTime(targetClatter, now, 0.04)
    clatter?.osc.frequency.setTargetAtTime(Math.max(18, Math.min(180, (state.rumbleHz || 80) * 0.45)), now, 0.05)

    scrape?.level.gain.setTargetAtTime(targetScrape, now, 0.03)
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
      case 'smash':
        // Brick build shattering: a thud plus a quick cascade of plastic clacks
        engine.playTone(140, 0.12, 0, 'sawtooth', 0.2 * intensity)
        engine.playTone(1180, 0.04, 0.0, 'square', 0.12 * intensity)
        engine.playTone(860, 0.05, 0.05, 'triangle', 0.14 * intensity)
        engine.playTone(1320, 0.04, 0.1, 'square', 0.1 * intensity)
        engine.playTone(700, 0.06, 0.16, 'triangle', 0.1 * intensity)
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
