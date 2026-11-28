// The base cards of the car editor show live pictures of each starter base,
// drawn from its real parts in the catalog (`thumbnails.js`): the model
// pictures have a queue of their own, so they never wait behind the dock's part
// pictures, and every starter base has a bundled placeholder for the moment
// before its live picture arrives.

import { test } from 'node:test'
import assert from 'node:assert/strict'
import { existsSync, readdirSync } from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { initGameThumbnails } from './thumbnails.js'

const here = path.dirname(fileURLToPath(import.meta.url))

function port() {
  const listeners = []
  return {
    subscribe: (f) => listeners.push(f),
    send: (v) => listeners.forEach((f) => f(v)),
    listeners,
  }
}

test('a base picture is not held up by part pictures that are still loading', async () => {
  const fetched = []
  const realFetch = globalThis.fetch
  // Part files never answer; the base's own brick does.
  globalThis.fetch = (url) => {
    fetched.push(String(url))
    if (String(url).includes('basebrick')) {
      return Promise.resolve(new Response('0 Hull brick\n', { status: 200, headers: { 'content-type': 'text/plain' } }))
    }
    return new Promise(() => {})
  }
  try {
    const ports = {
      requestThumbnails: port(),
      thumbnailLoaded: port(),
      requestModelThumbnails: port(),
      modelThumbnailLoaded: port(),
    }
    await initGameThumbnails(ports, 'https://catalog.test/raw')
    ports.requestThumbnails.send([{ file: 'stuckpart.dat', color: 4, rgb: [1, 0, 0] }])
    ports.requestModelThumbnails.send([
      { key: 'base:test', text: '1 70 0 0 0 1 0 0 0 1 0 0 0 1 basebrick.dat', colours: [] },
    ])
    await new Promise((r) => setTimeout(r, 50))
    assert.ok(
      fetched.some((u) => u.includes('basebrick.dat')),
      `the base's brick was fetched from the catalog (fetched: ${fetched.join(', ')})`,
    )
  } finally {
    globalThis.fetch = realFetch
  }
})

test('only the ports an app declares are wired', async () => {
  const models = { requestModelThumbnails: port(), modelThumbnailLoaded: port() }
  await initGameThumbnails(models, 'https://catalog.test/raw')
  assert.equal(models.requestModelThumbnails.listeners.length, 1)
  await initGameThumbnails({}, 'https://catalog.test/raw')
})

test('every starter base has a bundled placeholder picture', () => {
  const bases = readdirSync(path.join(here, 'starters/racer')).filter((f) => f.endsWith('.mpd'))
  assert.ok(bases.length > 0)
  for (const file of bases) {
    const png = path.join(here, 'public/base-previews', file.replace(/\.mpd$/, '.png'))
    assert.ok(existsSync(png), `missing ${path.relative(here, png)}`)
  }
})
