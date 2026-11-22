// Steering mode of Brick Racer: tilt or on-screen buttons.
//
// The shared `tilt-steer.js` turns tilt on by default on phones
// (`(pointer: coarse)`) and remembers the player's own choice under this
// localStorage key ('tilt' or 'touch'), which always wins over the default.
// Keep the key stable: changing it would forget every saved choice.
export const STEER_MODE_KEY = 'racer.steerMode'

export const tiltSteerOptions = { storageKey: STEER_MODE_KEY }
