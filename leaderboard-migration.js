// One-time cleanup of stored best times for the retired mountain courses.
//
// The retired course ids still parse (they map to the short courses so old
// share links keep working), which means best times saved on the long
// courses would otherwise show up as unbeatable records on the short ones.
// On first load after this change we drop those entries once and set a
// versioned flag so the migration never runs again.

export const RETIRED_TRACK_IDS = ['pine-ridge', 'quarry-switchbacks', 'alpine-glacier']

export const MIGRATION_FLAG_KEY = 'palikkakehys.racer.migrations.retiredTracks'
export const MIGRATION_VERSION = '1'

export function leaderboardKey(storagePrefix) {
  return `palikkakehys.${storagePrefix}.leaderboard`
}

// Pure: return the leaderboard JSON without entries on retired courses, or
// null when there is nothing to change (missing, malformed or already clean).
export function dropRetiredTrackEntries(rawJson) {
  if (typeof rawJson !== 'string' || rawJson === '') return null
  let entries
  try {
    entries = JSON.parse(rawJson)
  } catch {
    return null
  }
  if (!Array.isArray(entries)) return null
  const kept = entries.filter(
    (e) => !(e && typeof e === 'object' && RETIRED_TRACK_IDS.includes(e.trackId)),
  )
  if (kept.length === entries.length) return null
  return JSON.stringify(kept)
}

// Run the migration against a Storage-like object (defaults to
// window.localStorage). Returns true when stored entries were removed.
// Never throws: storage can be unavailable (private mode, blocked cookies).
export function migrateRetiredTrackBestTimes(storagePrefix = 'racer', storage) {
  let store = storage
  try {
    if (!store) store = window.localStorage
    if (!store) return false
    if (store.getItem(MIGRATION_FLAG_KEY) === MIGRATION_VERSION) return false
  } catch {
    return false
  }

  let changed = false
  try {
    const key = leaderboardKey(storagePrefix)
    const next = dropRetiredTrackEntries(store.getItem(key))
    if (next !== null) {
      store.setItem(key, next)
      changed = true
    }
  } catch {
    // Leave the flag unset so a later load can try again.
    return false
  }

  try {
    store.setItem(MIGRATION_FLAG_KEY, MIGRATION_VERSION)
  } catch {
    // Ignore: the cleanup is idempotent, so a repeat run is harmless.
  }
  return changed
}
