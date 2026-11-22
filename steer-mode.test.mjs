import { test } from 'node:test'
import assert from 'node:assert/strict'
import { STEER_MODE_KEY, tiltSteerOptions } from './steer-mode.js'
import {
  defaultTiltOn,
  resolveMode,
} from './vendor/packages/brick-frame-game/runtime/tilt-steer.js'

const fakeWindow = (coarse) => ({
  matchMedia: (query) => ({ matches: coarse && query === '(pointer: coarse)' }),
})

test('the steer mode storage key stays the same', () => {
  assert.equal(STEER_MODE_KEY, 'racer.steerMode')
  assert.deepEqual(tiltSteerOptions, { storageKey: 'racer.steerMode' })
})

test('tilt is the default steering on a touch phone', () => {
  assert.equal(defaultTiltOn(fakeWindow(true)), true)
  assert.equal(resolveMode(null, defaultTiltOn(fakeWindow(true))), true)
})

test('desktop starts with buttons and keys', () => {
  assert.equal(defaultTiltOn(fakeWindow(false)), false)
  assert.equal(resolveMode(null, defaultTiltOn(fakeWindow(false))), false)
})

test('a saved choice wins over the phone default', () => {
  assert.equal(resolveMode('touch', true), false)
  assert.equal(resolveMode('tilt', false), true)
})
