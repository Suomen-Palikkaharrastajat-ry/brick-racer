import assert from 'node:assert/strict'
import test from 'node:test'
import {
  MIGRATION_FLAG_KEY,
  MIGRATION_VERSION,
  dropRetiredTrackEntries,
  leaderboardKey,
  migrateRetiredTrackBestTimes,
} from './leaderboard-migration.js'

function memoryStorage(initial = {}) {
  const data = new Map(Object.entries(initial))
  return {
    data,
    getItem: (k) => (data.has(k) ? data.get(k) : null),
    setItem: (k, v) => data.set(k, String(v)),
  }
}

const entry = (trackId, timeSeconds) => ({
  trackId,
  vehicleName: 'Plank',
  timeSeconds,
  topSpeedKmh: 30,
  wheelCount: 4,
  steeringLabel: 'Turntable',
  seed: 1,
})

const KEY = leaderboardKey('racer')

test('drops best times on retired courses and keeps the rest', () => {
  const store = memoryStorage({
    [KEY]: JSON.stringify([
      entry('pine-ridge', 50),
      entry('farm-lane', 40),
      entry('quarry-switchbacks', 60),
      entry('alpine-glacier', 70),
      entry('forest-trail', 45),
    ]),
  })
  assert.equal(migrateRetiredTrackBestTimes('racer', store), true)
  const kept = JSON.parse(store.getItem(KEY)).map((e) => e.trackId)
  assert.deepEqual(kept, ['farm-lane', 'forest-trail'])
  assert.equal(store.getItem(MIGRATION_FLAG_KEY), MIGRATION_VERSION)
})

test('runs exactly once', () => {
  const store = memoryStorage({ [KEY]: JSON.stringify([entry('pine-ridge', 50)]) })
  assert.equal(migrateRetiredTrackBestTimes('racer', store), true)
  // A retired-course entry written after the migration is left alone.
  store.setItem(KEY, JSON.stringify([entry('alpine-glacier', 55)]))
  assert.equal(migrateRetiredTrackBestTimes('racer', store), false)
  assert.equal(JSON.parse(store.getItem(KEY)).length, 1)
})

test('sets the flag even when there is nothing to clean', () => {
  const store = memoryStorage()
  assert.equal(migrateRetiredTrackBestTimes('racer', store), false)
  assert.equal(store.getItem(KEY), null)
  assert.equal(store.getItem(MIGRATION_FLAG_KEY), MIGRATION_VERSION)
})

test('leaves malformed data untouched', () => {
  assert.equal(dropRetiredTrackEntries('not json'), null)
  assert.equal(dropRetiredTrackEntries('{"a":1}'), null)
  assert.equal(dropRetiredTrackEntries(JSON.stringify([entry('farm-lane', 1)])), null)
})

test('never throws when storage is unavailable', () => {
  const throwing = {
    getItem() {
      throw new Error('SecurityError')
    },
    setItem() {
      throw new Error('SecurityError')
    },
  }
  assert.equal(migrateRetiredTrackBestTimes('racer', throwing), false)

  const writeFails = memoryStorage({ [KEY]: JSON.stringify([entry('pine-ridge', 1)]) })
  writeFails.setItem = () => {
    throw new Error('QuotaExceededError')
  }
  assert.equal(migrateRetiredTrackBestTimes('racer', writeFails), false)
})
