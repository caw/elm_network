;(function SoundManager() {
const sounds = {
beep: new Howl({ src: ["beep2.mp3"]}),
};

window.SoundManager = function (name) {
const sound = sounds[name];
console.assert(sound != null, "Unknown sound: ${name}");
sound.play();
};
}());