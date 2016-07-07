/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId])
/******/ 			return installedModules[moduleId].exports;
/******/
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			exports: {},
/******/ 			id: moduleId,
/******/ 			loaded: false
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.loaded = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(0);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";
	__webpack_require__(15);
	__webpack_require__(16);
	const Application_1 = __webpack_require__(29);
	new Application_1.default();


/***/ },
/* 1 */
/***/ function(module, exports) {

	"use strict";
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.default = {
	    MAX_NOTES: 128,
	    ONE_MILLISECOND: 0.001,
	    FILTER_TYPE: {
	        HIGHPASS: 'highpass',
	        LOWPASS: 'lowpass',
	        BANDPASS: 'bandpass',
	        NOTCH: 'notch'
	    },
	    FILTER_TYPES: ['highpass', 'lowpass', 'bandpass', 'notch'],
	    WAVEFORM_TYPE: {
	        SAWTOOTH: 'sawtooth',
	        TRIANGLE: 'triangle',
	        SINE: 'sine',
	        SQUARE: 'square',
	        NOISE: 'whitenoise',
	        PULSE: 'pulse'
	    },
	    OSC1_WAVEFORM_TYPES: ['sine', 'triangle', 'sawtooth', 'square'],
	    OSC2_WAVEFORM_TYPES: ['triangle', 'sawtooth', 'pulse', 'whitenoise'],
	    MIDI_EVENT: {
	        NOTE_ON: 144,
	        NOTE_OFF: 128
	    },
	    MAX_ENVELOPE_TIME: 4,
	    OVERDRIVE_PARAMS: {
	        preBand: 1.0,
	        color: 4000,
	        drive: .8,
	        postCut: 8000
	    },
	    MIN_FILTER_FREQUENCY: 20,
	    MAX_FILTER_FREQUENCY: 12000 //20070
	};


/***/ },
/* 2 */
/***/ function(module, exports) {

	"use strict";
	// all midi values are integers between 0 and 127
	const MIDI_MAX_VALUE = 127;
	const midiToFreq = (midiValue) => (440 * Math.pow(2, (midiValue - 69) / 12));
	const manageMidiDevices = 
	// FIXME: type that shit
	// FIXME: type that shit
	    (onMIDIMessage, midiAccess, midiPort, midiStateChangePort) => {
	    let midiConnection = false;
	    // loop over all available inputs and listen for any MIDI input
	    for (const input of midiAccess.inputs.values()) {
	        input.onmidimessage = (midiMessage) => {
	            const data = midiMessage.data;
	            onMIDIMessage(data);
	            midiPort.send([
	                data[0],
	                data[1] || null,
	                data[2] || null
	            ]);
	        };
	        if (input.manufacturer !== '') {
	            midiConnection = true;
	        }
	    }
	    // this pernicious hack is necessary, see
	    // https://github.com/elm-lang/core/issues/595
	    setTimeout(() => midiStateChangePort.send(midiConnection), 0);
	    //console.log("mandei", midiConnection)
	};
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.default = {
	    // functions below scales midi values to synth parameters
	    toOscPitch: midiToFreq,
	    toFilterQAmount: (midiValue) => (20 * (midiValue / MIDI_MAX_VALUE)),
	    toFilterCutoffFrequency: (midiValue) => {
	        return 1.6 * midiToFreq(midiValue);
	        //midiValue / 127 *
	        //	(CONSTANTS.MAX_FILTER_FREQUENCY - CONSTANTS.MIN_FILTER_FREQUENCY) +
	        //	CONSTANTS.MIN_FILTER_FREQUENCY
	    },
	    logScaleToMax: (midiValue, max) => ((Math.pow(2, midiValue / MIDI_MAX_VALUE) - 1) * max),
	    normalizeValue: (midiValue) => (midiValue / MIDI_MAX_VALUE),
	    manageMidiDevices: (onMIDIMessage, midiAccess, midiPort, midiStateChange) => {
	        midiAccess.onstatechange = () => {
	            manageMidiDevices(onMIDIMessage, midiAccess, midiPort, midiStateChange);
	        };
	        manageMidiDevices(onMIDIMessage, midiAccess, midiPort, midiStateChange);
	    }
	};


/***/ },
/* 3 */
/***/ function(module, exports) {

	/*
		MIT License http://www.opensource.org/licenses/mit-license.php
		Author Tobias Koppers @sokra
	*/
	// css base code, injected by the css-loader
	module.exports = function() {
		var list = [];
	
		// return the list of modules as css string
		list.toString = function toString() {
			var result = [];
			for(var i = 0; i < this.length; i++) {
				var item = this[i];
				if(item[2]) {
					result.push("@media " + item[2] + "{" + item[1] + "}");
				} else {
					result.push(item[1]);
				}
			}
			return result.join("");
		};
	
		// import a list of modules into the list
		list.i = function(modules, mediaQuery) {
			if(typeof modules === "string")
				modules = [[null, modules, ""]];
			var alreadyImportedModules = {};
			for(var i = 0; i < this.length; i++) {
				var id = this[i][0];
				if(typeof id === "number")
					alreadyImportedModules[id] = true;
			}
			for(i = 0; i < modules.length; i++) {
				var item = modules[i];
				// skip already imported module
				// this implementation is not 100% perfect for weird media query combinations
				//  when a module is imported multiple times with different media queries.
				//  I hope this will never occur (Hey this way we have smaller bundles)
				if(typeof item[0] !== "number" || !alreadyImportedModules[item[0]]) {
					if(mediaQuery && !item[2]) {
						item[2] = mediaQuery;
					} else if(mediaQuery) {
						item[2] = "(" + item[2] + ") and (" + mediaQuery + ")";
					}
					list.push(item);
				}
			}
		};
		return list;
	};


/***/ },
/* 4 */
/***/ function(module, exports, __webpack_require__) {

	module.exports = __webpack_require__.p + "c88235ee2a75644c98f0edf16d6c6991.eot";

/***/ },
/* 5 */
/***/ function(module, exports, __webpack_require__) {

	/*
		MIT License http://www.opensource.org/licenses/mit-license.php
		Author Tobias Koppers @sokra
	*/
	var stylesInDom = {},
		memoize = function(fn) {
			var memo;
			return function () {
				if (typeof memo === "undefined") memo = fn.apply(this, arguments);
				return memo;
			};
		},
		isOldIE = memoize(function() {
			return /msie [6-9]\b/.test(window.navigator.userAgent.toLowerCase());
		}),
		getHeadElement = memoize(function () {
			return document.head || document.getElementsByTagName("head")[0];
		}),
		singletonElement = null,
		singletonCounter = 0,
		styleElementsInsertedAtTop = [];
	
	module.exports = function(list, options) {
		if(false) {
			if(typeof document !== "object") throw new Error("The style-loader cannot be used in a non-browser environment");
		}
	
		options = options || {};
		// Force single-tag solution on IE6-9, which has a hard limit on the # of <style>
		// tags it will allow on a page
		if (typeof options.singleton === "undefined") options.singleton = isOldIE();
	
		// By default, add <style> tags to the bottom of <head>.
		if (typeof options.insertAt === "undefined") options.insertAt = "bottom";
	
		var styles = listToStyles(list);
		addStylesToDom(styles, options);
	
		return function update(newList) {
			var mayRemove = [];
			for(var i = 0; i < styles.length; i++) {
				var item = styles[i];
				var domStyle = stylesInDom[item.id];
				domStyle.refs--;
				mayRemove.push(domStyle);
			}
			if(newList) {
				var newStyles = listToStyles(newList);
				addStylesToDom(newStyles, options);
			}
			for(var i = 0; i < mayRemove.length; i++) {
				var domStyle = mayRemove[i];
				if(domStyle.refs === 0) {
					for(var j = 0; j < domStyle.parts.length; j++)
						domStyle.parts[j]();
					delete stylesInDom[domStyle.id];
				}
			}
		};
	}
	
	function addStylesToDom(styles, options) {
		for(var i = 0; i < styles.length; i++) {
			var item = styles[i];
			var domStyle = stylesInDom[item.id];
			if(domStyle) {
				domStyle.refs++;
				for(var j = 0; j < domStyle.parts.length; j++) {
					domStyle.parts[j](item.parts[j]);
				}
				for(; j < item.parts.length; j++) {
					domStyle.parts.push(addStyle(item.parts[j], options));
				}
			} else {
				var parts = [];
				for(var j = 0; j < item.parts.length; j++) {
					parts.push(addStyle(item.parts[j], options));
				}
				stylesInDom[item.id] = {id: item.id, refs: 1, parts: parts};
			}
		}
	}
	
	function listToStyles(list) {
		var styles = [];
		var newStyles = {};
		for(var i = 0; i < list.length; i++) {
			var item = list[i];
			var id = item[0];
			var css = item[1];
			var media = item[2];
			var sourceMap = item[3];
			var part = {css: css, media: media, sourceMap: sourceMap};
			if(!newStyles[id])
				styles.push(newStyles[id] = {id: id, parts: [part]});
			else
				newStyles[id].parts.push(part);
		}
		return styles;
	}
	
	function insertStyleElement(options, styleElement) {
		var head = getHeadElement();
		var lastStyleElementInsertedAtTop = styleElementsInsertedAtTop[styleElementsInsertedAtTop.length - 1];
		if (options.insertAt === "top") {
			if(!lastStyleElementInsertedAtTop) {
				head.insertBefore(styleElement, head.firstChild);
			} else if(lastStyleElementInsertedAtTop.nextSibling) {
				head.insertBefore(styleElement, lastStyleElementInsertedAtTop.nextSibling);
			} else {
				head.appendChild(styleElement);
			}
			styleElementsInsertedAtTop.push(styleElement);
		} else if (options.insertAt === "bottom") {
			head.appendChild(styleElement);
		} else {
			throw new Error("Invalid value for parameter 'insertAt'. Must be 'top' or 'bottom'.");
		}
	}
	
	function removeStyleElement(styleElement) {
		styleElement.parentNode.removeChild(styleElement);
		var idx = styleElementsInsertedAtTop.indexOf(styleElement);
		if(idx >= 0) {
			styleElementsInsertedAtTop.splice(idx, 1);
		}
	}
	
	function createStyleElement(options) {
		var styleElement = document.createElement("style");
		styleElement.type = "text/css";
		insertStyleElement(options, styleElement);
		return styleElement;
	}
	
	function createLinkElement(options) {
		var linkElement = document.createElement("link");
		linkElement.rel = "stylesheet";
		insertStyleElement(options, linkElement);
		return linkElement;
	}
	
	function addStyle(obj, options) {
		var styleElement, update, remove;
	
		if (options.singleton) {
			var styleIndex = singletonCounter++;
			styleElement = singletonElement || (singletonElement = createStyleElement(options));
			update = applyToSingletonTag.bind(null, styleElement, styleIndex, false);
			remove = applyToSingletonTag.bind(null, styleElement, styleIndex, true);
		} else if(obj.sourceMap &&
			typeof URL === "function" &&
			typeof URL.createObjectURL === "function" &&
			typeof URL.revokeObjectURL === "function" &&
			typeof Blob === "function" &&
			typeof btoa === "function") {
			styleElement = createLinkElement(options);
			update = updateLink.bind(null, styleElement);
			remove = function() {
				removeStyleElement(styleElement);
				if(styleElement.href)
					URL.revokeObjectURL(styleElement.href);
			};
		} else {
			styleElement = createStyleElement(options);
			update = applyToTag.bind(null, styleElement);
			remove = function() {
				removeStyleElement(styleElement);
			};
		}
	
		update(obj);
	
		return function updateStyle(newObj) {
			if(newObj) {
				if(newObj.css === obj.css && newObj.media === obj.media && newObj.sourceMap === obj.sourceMap)
					return;
				update(obj = newObj);
			} else {
				remove();
			}
		};
	}
	
	var replaceText = (function () {
		var textStore = [];
	
		return function (index, replacement) {
			textStore[index] = replacement;
			return textStore.filter(Boolean).join('\n');
		};
	})();
	
	function applyToSingletonTag(styleElement, index, remove, obj) {
		var css = remove ? "" : obj.css;
	
		if (styleElement.styleSheet) {
			styleElement.styleSheet.cssText = replaceText(index, css);
		} else {
			var cssNode = document.createTextNode(css);
			var childNodes = styleElement.childNodes;
			if (childNodes[index]) styleElement.removeChild(childNodes[index]);
			if (childNodes.length) {
				styleElement.insertBefore(cssNode, childNodes[index]);
			} else {
				styleElement.appendChild(cssNode);
			}
		}
	}
	
	function applyToTag(styleElement, obj) {
		var css = obj.css;
		var media = obj.media;
	
		if(media) {
			styleElement.setAttribute("media", media)
		}
	
		if(styleElement.styleSheet) {
			styleElement.styleSheet.cssText = css;
		} else {
			while(styleElement.firstChild) {
				styleElement.removeChild(styleElement.firstChild);
			}
			styleElement.appendChild(document.createTextNode(css));
		}
	}
	
	function updateLink(linkElement, obj) {
		var css = obj.css;
		var sourceMap = obj.sourceMap;
	
		if(sourceMap) {
			// http://stackoverflow.com/a/26603875
			css += "\n/*# sourceMappingURL=data:application/json;base64," + btoa(unescape(encodeURIComponent(JSON.stringify(sourceMap)))) + " */";
		}
	
		var blob = new Blob([css], { type: "text/css" });
	
		var oldSrc = linkElement.href;
	
		linkElement.href = URL.createObjectURL(blob);
	
		if(oldSrc)
			URL.revokeObjectURL(oldSrc);
	}


/***/ },
/* 6 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";
	const MIDI_1 = __webpack_require__(2);
	const Constants_1 = __webpack_require__(1);
	class ADSR {
	    constructor(context, state) {
	        this.setState = (state) => {
	            this.state = state;
	            this.state.attack = this.state.attack || Constants_1.default.ONE_MILLISECOND;
	            this.state.release = this.state.release || Constants_1.default.ONE_MILLISECOND;
	        };
	        this.getValue = (start, end, fromTime, toTime, at) => {
	            const difference = end - start;
	            const time = toTime - fromTime;
	            const truncateTime = at - fromTime;
	            const phase = truncateTime / time;
	            let value = start + phase * difference;
	            if (difference >= 0) {
	                if (value <= start) {
	                    value = start;
	                }
	                if (value >= end) {
	                    value = end;
	                }
	            }
	            else {
	                if (value >= start) {
	                    value = start;
	                }
	                if (value <= end) {
	                    value = end;
	                }
	            }
	            return value;
	        };
	        this.getValueAtTime = (now) => {
	            let valueAtTime = this.sustainAmount;
	            if (now >= this.state.attack && now < this.decayFrom) {
	                valueAtTime = this.getValue(this.startAmount, this.endAmount, this.startedAt, this.decayFrom, now);
	            }
	            else if (now >= this.decayFrom && now < this.decayTo) {
	                valueAtTime = this.getValue(this.endAmount, this.sustainAmount, this.decayFrom, this.decayTo, now);
	            }
	            return valueAtTime;
	        };
	        this.on = (startAmount, endAmount) => (target) => {
	            const now = this.context.currentTime;
	            this.startedAt = now;
	            this.decayFrom = this.startedAt + this.state.attack;
	            this.decayTo = this.decayFrom + this.state.decay;
	            this.startAmount = startAmount;
	            this.endAmount = endAmount;
	            this.sustainAmount = this.state.sustain *
	                (this.endAmount - this.startAmount) + this.startAmount;
	            //debugger
	            target.cancelScheduledValues(now);
	            target.setValueAtTime(this.startAmount, now);
	            target.linearRampToValueAtTime(this.endAmount, this.decayFrom);
	            target.linearRampToValueAtTime(this.sustainAmount, this.decayTo);
	        };
	        this.off = (target) => {
	            const now = this.context.currentTime;
	            const valueAtTime = this.getValueAtTime(now);
	            target.cancelScheduledValues(now);
	            target.setValueAtTime(valueAtTime, now);
	            target.linearRampToValueAtTime(this.startAmount, now + this.state.release);
	            return now + this.state.release;
	        };
	        this.setAttack = (midiValue) => {
	            this.state.attack = midiValue != 0 ?
	                MIDI_1.default.logScaleToMax(midiValue, Constants_1.default.MAX_ENVELOPE_TIME) :
	                Constants_1.default.ONE_MILLISECOND;
	        };
	        this.setDecay = (midiValue) => {
	            this.state.decay = MIDI_1.default.logScaleToMax(midiValue, Constants_1.default.MAX_ENVELOPE_TIME);
	        };
	        this.setSustain = (midiValue) => {
	            this.state.sustain = MIDI_1.default.logScaleToMax(midiValue, 1);
	        };
	        this.setRelease = (midiValue) => {
	            this.state.release = midiValue != 0 ?
	                MIDI_1.default.logScaleToMax(midiValue, Constants_1.default.MAX_ENVELOPE_TIME) :
	                Constants_1.default.ONE_MILLISECOND;
	        };
	        this.startAmount = 0;
	        this.sustainAmount = 0;
	        this.endAmount = 1;
	        this.context = context;
	        this.startedAt = 0;
	        this.decayFrom = 0;
	        this.decayTo = 0;
	        this.setState(state);
	    }
	}
	exports.ADSR = ADSR;


/***/ },
/* 7 */
/***/ function(module, exports) {

	"use strict";
	class BaseOscillator {
	    constructor(context, waveformType) {
	        this.panic = () => {
	            for (const midiNote in this.voices) {
	                if (this.voices.hasOwnProperty(midiNote)) {
	                    this.voices[midiNote].stop();
	                }
	            }
	        };
	        this.frequencyFromNoteNumber = (note) => {
	            const note_ = this.kbdTrack ? note : 60;
	            return 440 * Math.pow(2, (note_ - 69) / 12);
	        };
	        this.noteOn = (midiNote, noteOnAmpCB) => {
	            const midiNoteKey = midiNote.toString();
	            const now = this.context.currentTime;
	            if (midiNoteKey in this.voices) {
	                this.voices[midiNoteKey].stop(now);
	                this.frequencyGains[midiNote].disconnect();
	                this.voices[midiNoteKey].disconnect();
	                delete this.voices[midiNoteKey];
	            }
	            this._noteOn(midiNote);
	            this.voices[midiNoteKey].onended = () => {
	                this._onended(this.voices[midiNoteKey]);
	                this.voices[midiNoteKey].disconnect();
	                delete this.voices[midiNoteKey];
	            };
	            this.voices[midiNoteKey].start(now);
	            if (noteOnAmpCB) {
	                noteOnAmpCB(this.voiceGains[midiNote].gain);
	            }
	        };
	        this.noteOff = (midiNote, noteOffAmpCB) => {
	            const midiNoteKey = midiNote.toString();
	            const osc = this.voices[midiNoteKey];
	            if (!osc) {
	                return;
	            }
	            const voiceGain = this.voiceGains[midiNote].gain;
	            const releaseTime = noteOffAmpCB(voiceGain);
	            osc.stop(releaseTime);
	        };
	        this.setKbdTrack = (state) => {
	            this.kbdTrack = state;
	        };
	        this.context = context;
	        this.output = this.context.createGain();
	        this.output.gain.value = .5;
	        this.voices = {};
	        this.voiceGains = [];
	        this.frequencyGains = [];
	        this.kbdTrack = true;
	        this.type = waveformType;
	        for (let i = 0; i < 128; i++) {
	            //TODO: create FM osc and let it holdd the gains,
	            // instead of the modular like it is
	            this.frequencyGains[i] = this.context.createGain();
	            this.voiceGains[i] = this.context.createGain();
	            this.voiceGains[i].connect(this.output);
	        }
	    }
	    setSemitone(semitone) { }
	    setDetune(detune) { }
	    setPulseWidth(pw) { }
	    setWaveform(waveform) { }
	    _onended(voice) { }
	    connect(node) {
	        this.output.connect(node);
	        return this;
	    }
	    disconnect(node) {
	        this.output.disconnect(node);
	        return this;
	    }
	}
	exports.BaseOscillator = BaseOscillator;


/***/ },
/* 8 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";
	const BaseOscillator_1 = __webpack_require__(7);
	class FMOscillator extends BaseOscillator_1.BaseOscillator {
	    constructor(context, waveformType) {
	        super(context, waveformType);
	        this.setFMGain = (fmGain) => {
	            for (let i = 0; i < 128; i++) {
	                this.frequencyGains[i].gain.value = fmGain;
	            }
	        };
	        this.detune = 0;
	        this.semitone = 0;
	    }
	    _noteOn(midiNote) {
	        const midiNoteKey = midiNote.toString();
	        const osc = this.context.createOscillator();
	        osc.type = this.type;
	        osc.frequency.value = this.frequencyFromNoteNumber(midiNote);
	        osc.detune.value = this.detune + this.semitone;
	        osc.connect(this.voiceGains[midiNote]);
	        this.frequencyGains[midiNote].connect(osc.frequency);
	        this.voices[midiNoteKey] = osc;
	    }
	    setDetune(detune) {
	        this.detune = detune;
	        for (const midiNote in this.voices) {
	            if (this.voices.hasOwnProperty(midiNote)) {
	                this.voices[midiNote].detune.value =
	                    detune + this.semitone;
	            }
	        }
	    }
	    setSemitone(semitone) {
	        this.semitone = semitone * 100;
	        for (const midiNote in this.voices) {
	            if (this.voices.hasOwnProperty(midiNote)) {
	                this.voices[midiNote].detune.value =
	                    this.detune + this.semitone;
	            }
	        }
	    }
	    setWaveform(waveform) {
	        for (const midiNote in this.voices) {
	            if (this.voices.hasOwnProperty(midiNote)) {
	                this.voices[midiNote].type = waveform;
	            }
	        }
	        this.type = waveform;
	    }
	}
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.default = FMOscillator;


/***/ },
/* 9 */
/***/ function(module, exports, __webpack_require__) {

	exports = module.exports = __webpack_require__(3)();
	// imports
	
	
	// module
	exports.push([module.id, "/*! normalize.css v4.1.1 | MIT License | github.com/necolas/normalize.css */html{font-family:sans-serif;line-height:1.15;-ms-text-size-adjust:100%;-webkit-text-size-adjust:100%}body{margin:0}article,aside,details,figcaption,figure,footer,header,main,menu,nav,section,summary{display:block}audio,canvas,progress,video{display:inline-block}audio:not([controls]){display:none;height:0}progress{vertical-align:baseline}[hidden],template{display:none}a{background-color:transparent;-webkit-text-decoration-skip:objects}a:active,a:hover{outline-width:0}abbr[title]{border-bottom:none;text-decoration:underline;text-decoration:underline dotted}b,strong{font-weight:inherit;font-weight:bolder}dfn{font-style:italic}h1{font-size:2em;margin:.67em 0}mark{background-color:#ff0;color:#000}small{font-size:80%}sub,sup{font-size:75%;line-height:0;position:relative;vertical-align:baseline}sub{bottom:-.25em}sup{top:-.5em}img{border-style:none}svg:not(:root){overflow:hidden}code,kbd,pre,samp{font-family:monospace,monospace;font-size:1em}figure{margin:1em 40px}hr{box-sizing:content-box;height:0;overflow:visible}button,input,optgroup,select,textarea{font:inherit;margin:0}optgroup{font-weight:700}button,input{overflow:visible}button,select{text-transform:none}[type=reset],[type=submit],button,html [type=button]{-webkit-appearance:button}[type=button]::-moz-focus-inner,[type=reset]::-moz-focus-inner,[type=submit]::-moz-focus-inner,button::-moz-focus-inner{border-style:none;padding:0}[type=button]:-moz-focusring,[type=reset]:-moz-focusring,[type=submit]:-moz-focusring,button:-moz-focusring{outline:1px dotted ButtonText}fieldset{border:1px solid silver;margin:0 2px;padding:.35em .625em .75em}legend{box-sizing:border-box;color:inherit;display:table;max-width:100%;padding:0;white-space:normal}textarea{overflow:auto}[type=checkbox],[type=radio]{box-sizing:border-box;padding:0}[type=number]::-webkit-inner-spin-button,[type=number]::-webkit-outer-spin-button{height:auto}[type=search]{-webkit-appearance:textfield;outline-offset:-2px}[type=search]::-webkit-search-cancel-button,[type=search]::-webkit-search-decoration{-webkit-appearance:none}::-webkit-input-placeholder{color:inherit;opacity:.54}::-webkit-file-upload-button{-webkit-appearance:button;font:inherit}", ""]);
	
	// exports


/***/ },
/* 10 */
/***/ function(module, exports, __webpack_require__) {

	exports = module.exports = __webpack_require__(3)();
	// imports
	
	
	// module
	exports.push([module.id, "@font-face{font-family:Audiowide;font-style:normal;font-weight:400;src:local('Audiowide'),local('Audiowide-Regular'),url(\"https://fonts.gstatic.com/s/audiowide/v4/8XtYtNKEyyZh481XVWfVOpBw1xU1rKptJj_0jans920.woff2\") format('woff2');unicode-range:U+0000-00FF,U+0131,U+0152-0153,U+02C6,U+02DA,U+02DC,U+2000-206F,U+2074,U+20AC,U+2212,U+2215,U+E0FF,U+EFFD,U+F000}@font-face{font-family:Poiret One;font-style:normal;font-weight:400;src:local('Poiret One'),local('PoiretOne-Regular'),url(\"https://fonts.gstatic.com/s/poiretone/v4/HrI4ZJpJ3Fh0wa5ofYMK8QzyDMXhdD8sAj6OAJTFsBI.woff2\") format('woff2');unicode-range:U+0000-00FF,U+0131,U+0152-0153,U+02C6,U+02DA,U+02DC,U+2000-206F,U+2074,U+20AC,U+2212,U+2215,U+E0FF,U+EFFD,U+F000}@font-face{font-family:Maven Pro;font-style:normal;font-weight:400;src:local('Maven Pro Regular'),local('MavenProRegular'),url(\"https://fonts.gstatic.com/s/mavenpro/v7/MG9KbUZFchDs94Tbv9U-pZBw1xU1rKptJj_0jans920.woff2\") format('woff2');unicode-range:U+0000-00FF,U+0131,U+0152-0153,U+02C6,U+02DA,U+02DC,U+2000-206F,U+2074,U+20AC,U+2212,U+2215,U+E0FF,U+EFFD,U+F000}@font-face{font-family:Digital-7 Mono;src:url(" + __webpack_require__(4) + ");src:url(" + __webpack_require__(4) + "?#iefix) format(\"embedded-opentype\"),url(" + __webpack_require__(13) + ") format(\"woff\"),url(" + __webpack_require__(12) + ") format(\"truetype\");font-weight:400;font-style:normal}body{margin:0;height:100vh;background-color:#ad131d;cursor:default;font-family:Helvetica Neue,Helvetica,sans-serif;-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none}.dashboard{height:100vh;width:100vw;display:-webkit-box;display:-ms-flexbox;display:flex;-webkit-box-orient:vertical;-webkit-box-direction:normal;-ms-flex-direction:column;flex-direction:column;box-shadow:inset 3px 3px 10px 1px #450202;-webkit-box-pack:justify;-ms-flex-pack:justify;justify-content:space-between;-webkit-box-align:center;-ms-flex-align:center;align-items:center}.dashboard .virtual-keyboard{margin-top:auto}.keyboard{margin:1rem;padding:0;-webkit-box-flex:2;-ms-flex-positive:2;flex-grow:2;display:-webkit-box;display:-ms-flexbox;display:flex;-webkit-box-align:end;-ms-flex-align:end;align-items:flex-end;-webkit-box-pack:center;-ms-flex-pack:center;justify-content:center}.keyboard .keyboard__key{margin:0;padding:0;list-style:none;position:relative;float:left}.keyboard .keyboard__key.keyboard__key--lower{height:3rem;width:1rem;border-left:.03125rem solid;border-bottom:.03125rem solid #999;background:-webkit-linear-gradient(top,#eee,#fff);background:linear-gradient(180deg,#eee 0,#fff)}.keyboard .keyboard__key.keyboard__key--current-octave{background:#d3cdff}.keyboard .keyboard__key.keyboard__key--pressed{background:-webkit-linear-gradient(top,#fff,#e9e9e9);background:linear-gradient(180deg,#fff 0,#e9e9e9);box-shadow:inset 2px 0 3px rgba(0,0,0,.1),inset -5px 5px 20px rgba(0,0,0,.4),0 0 3px rgba(0,0,0,.4);background:#d3cdff}.keyboard .keyboard__key.keyboard__key--higher{height:1.5rem;width:.5rem;margin:0;margin-bottom:1.5rem;margin-left:-.25rem;z-index:2;border:1px solid #000;background:-webkit-linear-gradient(45deg,#222,#9e9e9e);background:linear-gradient(45deg,#222,#9e9e9e)}.keyboard .keyboard__key.keyboard__key--a,.keyboard .keyboard__key.keyboard__key--b,.keyboard .keyboard__key.keyboard__key--c,.keyboard .keyboard__key.keyboard__key--d,.keyboard .keyboard__key.keyboard__key--e,.keyboard .keyboard__key.keyboard__key--f,.keyboard .keyboard__key.keyboard__key--g{margin:0;margin-left:-.25rem}.keyboard .keyboard__key.keyboard__key--b,.keyboard .keyboard__key.keyboard__key--e{margin-right:.25rem}@media only screen and (max-width:1280px){.keyboard .keyboard__key.keyboard__key--full{display:none}}@media only screen and (max-width:1080px){.keyboard .keyboard__key.keyboard__key--big{display:none}}@media only screen and (max-width:898px){.keyboard .keyboard__key.keyboard__key--medium{display:none}}.information-bar{display:-webkit-box;display:-ms-flexbox;display:flex;-ms-flex-pack:distribute;justify-content:space-around;-webkit-box-align:center;-ms-flex-align:center;align-items:center;width:100vw;background-color:rgba(0,0,0,.2);color:#fff;font-family:monospace;padding:.5rem}.information-bar__gh-link,.information-bar__item,.midi-indicator{padding-right:1rem;padding-left:1rem}.information-bar__gh-link{height:1.6rem;padding-top:7px;background-image:url(" + __webpack_require__(19) + ");background-repeat:no-repeat;margin-top:3px;background-size:20px;background-position:50%}.panel-controls{display:-webkit-box;display:-ms-flexbox;display:flex;-ms-flex-pack:distribute;justify-content:space-around;color:#fff;background-color:#291c2d;border-radius:.5rem;margin:1rem;padding:1rem}.panel-controls__column{-webkit-box-flex:1;-ms-flex-positive:1;flex-grow:1;margin-right:1rem}.panel-controls__column:last-of-type{margin-right:0}.panel-controls__column:nth-of-type(2) .knob,.panel-controls__column:nth-of-type(2) .option-picker{width:20%}.panel-controls__column:nth-of-type(2) .knob:last-of-type{margin-top:1rem}.panel-controls__column:nth-of-type(2) .switch{width:40%}.panel-controls__column:nth-of-type(2) .section__content{padding-top:0}.panel-controls__column .section{margin-bottom:1rem}.panel-controls__column .section:last-of-type{margin-bottom:0}@media only screen and (max-width:930px){.panel-controls__column:last-of-class{margin-right:0}.panel-controls__instructions{display:none}}.panel__brand{display:-webkit-box;display:-ms-flexbox;display:flex;-ms-flex-pack:distribute;justify-content:space-around;margin:1rem;margin-top:0;padding:1rem;padding-top:0;color:#fff;margin-left:auto;font-size:3rem;line-height:2.6rem;overflow:auto}.panel__brand :nth-of-type(1){font-family:Poiret One,cursive;font-size:1.5em;font-weight:700}.panel__brand :nth-of-type(2){font-family:Audiowide,cursive;color:#291c2d}.panel__brand :nth-of-type(3){font-family:Maven Pro,sans-serif;display:block;font-size:30px;letter-spacing:22px;margin-right:-20px}.panel__brand :nth-of-type(3):after{display:inline-block;width:100%;content:''}.switch{margin:0 auto;text-align:center}.switch__list{list-style-type:none;display:-webkit-box;display:-ms-flexbox;display:flex;-webkit-box-orient:vertical;-webkit-box-direction:normal;-ms-flex-direction:column;flex-direction:column;margin-top:0;-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none}.switch__item,.switch__list{padding-left:4.5px;margin-bottom:4px}.switch__item--active,.switch__item--inactive,.switch__label{display:inline-block}.switch__item--active{box-shadow:0 0 1px 0 #000,inset -.6px -1.2px 3.6px 2px #6eb300,0 0 3px 0 #6eb300}.switch__item--active,.switch__item--inactive{width:6px;height:6px;background-color:#c5c5c5;border-radius:50%}.switch__item--inactive{box-shadow:0 0 1px 0 #000,inset -.6px -1.2px 3.6px 2px rgba(40,40,40,.75),0 0 3px 0 rgba(40,40,40,.75)}.switch__label{font-size:10px;margin-left:5px}.switch__btn{background-color:#4b4b4b;cursor:pointer;outline:0;width:45px;height:20.454545454545453px;border-radius:2px;border:6px solid #363636}.switch__btn:active{border-color:#000;background-color:#363636}.knob{text-align:center}.knob__scale{height:48px;width:48px;margin-left:auto;margin-right:auto;margin-bottom:2px;background:url(" + __webpack_require__(20) + ");background-size:48px;background-repeat:no-repeat;background-position:50%}.knob__dial{background:url(" + __webpack_require__(21) + ");background-position:50%;background-repeat:no-repeat;background-size:100%;width:100%;height:100%}.knob__dial,.knob__label{-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none}.knob__label{display:inline;font-size:70%;background-color:#787e9f;border-radius:.5rem;padding-right:.5rem;padding-left:.5rem;margin-left:auto;margin-right:auto;white-space:nowrap}.knob__adsr-label--attack{background:url(" + __webpack_require__(17) + ")}.knob__adsr-label--decay{background:url(" + __webpack_require__(18) + ")}.knob__adsr-label--sustain{background:url(" + __webpack_require__(27) + ")}.knob__adsr-label--release{background:url(" + __webpack_require__(23) + ")}.knob__adsr-label--attack,.knob__adsr-label--decay,.knob__adsr-label--release,.knob__adsr-label--sustain{width:2rem;height:1.34rem;margin-left:auto;margin-right:auto;margin-bottom:-.4rem;background-position:50%;background-repeat:no-repeat;background-size:100%}.section{background-color:#a9a9a9;color:#fff;border-radius:.5rem}.section--with-bevel .section__content{padding:0;-webkit-box-align:stretch;-ms-flex-align:stretch;-ms-grid-row-align:stretch;align-items:stretch}.section__content{display:-webkit-box;display:-ms-flexbox;display:flex;-ms-flex-wrap:wrap;flex-wrap:wrap;-webkit-box-pack:justify;-ms-flex-pack:justify;justify-content:space-between;-webkit-box-align:center;-ms-flex-align:center;align-items:center;padding:.5rem 1rem}.section__content--beveled{padding:0}.section__title{height:1rem;letter-spacing:1px;background-color:#787e9f;border-top-left-radius:inherit;border-top-right-radius:inherit;padding-bottom:1rem;font-family:monospace;padding-left:.4rem;-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none}.amplifier,.filter{display:-webkit-box;display:-ms-flexbox;display:flex;-webkit-box-orient:horizontal;-webkit-box-direction:normal;-ms-flex-direction:row;flex-direction:row;-webkit-box-pack:justify;-ms-flex-pack:justify;justify-content:space-between}.oscillators__extra,.oscillators__osc1,.oscillators__osc2{display:-webkit-box;display:-ms-flexbox;display:flex;text-align:center;padding-top:.5rem;padding-bottom:.5rem}.oscillators__osc1,.oscillators__osc2{-ms-flex-pack:distribute;justify-content:space-around}.oscillators__osc1{-webkit-box-orient:vertical;-webkit-box-direction:normal;-ms-flex-direction:column;flex-direction:column;width:35%;border-right:2px solid #787e9f}.oscillators__osc2{-webkit-box-orient:horizontal;-webkit-box-direction:normal;-ms-flex-direction:row;flex-direction:row;-ms-flex-wrap:wrap;flex-wrap:wrap;width:65%}.oscillators__osc2 .knob,.oscillators__osc2 .option-picker{width:50%}.oscillators__osc2 .oscillators__label{width:100%}.oscillators__extra{-webkit-box-flex:2;-ms-flex-positive:2;flex-grow:2;-ms-flex-pack:distribute;justify-content:space-around;border-top:2px solid #787e9f}.oscillators__label{font-weight:700;text-align:center;color:#787e9f;font-size:140%;-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;bottom:0}.option-picker{margin:0 auto;text-align:center}.option-picker__label{display:inline;font-size:70%;background-color:#787e9f;border-radius:.5rem;padding-right:.5rem;padding-left:.5rem;white-space:nowrap}.option-picker__label,.option-picker__list{-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none}.option-picker__list{list-style-type:none;padding-left:4.5px;width:3rem;margin:0 auto 4px}.option-picker__id--noise:before{content:'noise';font-size:10px;-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;margin-left:5px}.option-picker__id--pulse{background:url(" + __webpack_require__(22) + ")}.option-picker__id--tri{background:url(" + __webpack_require__(28) + ")}.option-picker__id--sin{background:url(" + __webpack_require__(25) + ")}.option-picker__id--saw{background:url(" + __webpack_require__(24) + ")}.option-picker__id--sqr{background:url(" + __webpack_require__(26) + ")}.option-picker__id--pulse,.option-picker__id--saw,.option-picker__id--sin,.option-picker__id--sqr,.option-picker__id--tri,user-select none{margin-left:5px;background-position:50%;background-repeat:no-repeat;background-size:100%;width:20px;height:20px}.option-picker__item{display:-webkit-box;display:-ms-flexbox;display:flex;-webkit-box-align:center;-ms-flex-align:center;align-items:center}.option-picker__item--active{box-shadow:0 0 1px 0 #000,inset -.6px -1.2px 3.6px 2px #6eb300,0 0 3px 0 #6eb300}.option-picker__item--active,.option-picker__item--inactive{width:6px;height:6px;background-color:#c5c5c5;border-radius:50%}.option-picker__item--inactive{box-shadow:0 0 1px 0 #000,inset -.6px -1.2px 3.6px 2px rgba(40,40,40,.75),0 0 3px 0 rgba(40,40,40,.75)}.option-picker__item-id{margin-left:5px;font-size:10px}.option-picker__btn{background-color:#4b4b4b;cursor:pointer;outline:0;width:45px;height:20.454545454545453px;border-radius:2px;border:6px solid #363636}.option-picker__btn:active{border-color:#000;background-color:#363636}.option-picker__id--BP,.option-picker__id--HP,.option-picker__id--LP,.option-picker__id--notch{font-size:10px;-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;margin-left:5px}.option-picker__id--HP:before{content:'HP'}.option-picker__id--BP:before{content:'BP'}.option-picker__id--LP:before{content:'LP'}.option-picker__id--notch:before{content:'notch'}.instructions{padding:0;font-size:70%}.instructions,.instructions__title{-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none}.instructions__title{text-align:center;width:100%}.instructions__label{padding-left:20px}.incrementer{-webkit-box-orient:horizontal;-ms-flex-direction:row;flex-direction:row;text-align:center;-webkit-box-align:center;-ms-flex-align:center;align-items:center}.incrementer,.incrementer__info{display:-webkit-box;display:-ms-flexbox;display:flex;-webkit-box-direction:normal}.incrementer__info{-webkit-box-orient:vertical;-ms-flex-direction:column;flex-direction:column;margin-right:.5rem}.incrementer__label{height:20.454545454545453px;line-height:20.454545454545453px;margin-bottom:.25rem}.incrementer__display{padding-left:.5rem;padding-right:.5rem;font-family:Digital-7 Mono,monospace;background:#510b0b;color:#bc2121;border-radius:6px;font-size:1.3em}.incrementer__display--patch{width:200px}.incrementer__buttons{width:45px}.incrementer__button{background-color:#4b4b4b;cursor:pointer;outline:0;width:45px;height:20.454545454545453px;border-radius:2px;border:6px solid #363636;margin-bottom:.25rem}.incrementer__button:active{border-color:#000;background-color:#363636}.incrementer__button:last-of-type{margin-bottom:0}.midi-indicator{display:-webkit-box;display:-ms-flexbox;display:flex;-webkit-box-orient:vertical;-webkit-box-direction:normal;-ms-flex-direction:column;flex-direction:column;text-align:center;padding-right:1rem;padding-left:1rem}.midi-indicator__blinker:before{content:\"MIDI\"}.midi-indicator__blinker--active{color:#34c945}.midi-indicator__status{padding-left:.5rem}.midi-indicator__status--active{color:#34c945}.midi-indicator__status--active:before{content:\"connected\"}.midi-indicator__status--inactive{color:#a8a8a8}.midi-indicator__status--inactive:before{content:\"not connected\"}.midi-indicator__status--not-supported{color:#ff4f4f}.midi-indicator__status--not-supported:before{content:\"not supported\"}.midi-indicator__status--scanning{color:#a8a8a8}.midi-indicator__status--scanning:before{content:\"scanning devices\"}*{box-sizing:border-box}", ""]);
	
	// exports


/***/ },
/* 11 */
/***/ function(module, exports, __webpack_require__) {

	var __WEBPACK_AMD_DEFINE_ARRAY__, __WEBPACK_AMD_DEFINE_RESULT__;/* WEBPACK VAR INJECTION */(function(module) {
	(function() {
	'use strict';
	
	function F2(fun)
	{
	  function wrapper(a) { return function(b) { return fun(a,b); }; }
	  wrapper.arity = 2;
	  wrapper.func = fun;
	  return wrapper;
	}
	
	function F3(fun)
	{
	  function wrapper(a) {
	    return function(b) { return function(c) { return fun(a, b, c); }; };
	  }
	  wrapper.arity = 3;
	  wrapper.func = fun;
	  return wrapper;
	}
	
	function F4(fun)
	{
	  function wrapper(a) { return function(b) { return function(c) {
	    return function(d) { return fun(a, b, c, d); }; }; };
	  }
	  wrapper.arity = 4;
	  wrapper.func = fun;
	  return wrapper;
	}
	
	function F5(fun)
	{
	  function wrapper(a) { return function(b) { return function(c) {
	    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
	  }
	  wrapper.arity = 5;
	  wrapper.func = fun;
	  return wrapper;
	}
	
	function F6(fun)
	{
	  function wrapper(a) { return function(b) { return function(c) {
	    return function(d) { return function(e) { return function(f) {
	    return fun(a, b, c, d, e, f); }; }; }; }; };
	  }
	  wrapper.arity = 6;
	  wrapper.func = fun;
	  return wrapper;
	}
	
	function F7(fun)
	{
	  function wrapper(a) { return function(b) { return function(c) {
	    return function(d) { return function(e) { return function(f) {
	    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
	  }
	  wrapper.arity = 7;
	  wrapper.func = fun;
	  return wrapper;
	}
	
	function F8(fun)
	{
	  function wrapper(a) { return function(b) { return function(c) {
	    return function(d) { return function(e) { return function(f) {
	    return function(g) { return function(h) {
	    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
	  }
	  wrapper.arity = 8;
	  wrapper.func = fun;
	  return wrapper;
	}
	
	function F9(fun)
	{
	  function wrapper(a) { return function(b) { return function(c) {
	    return function(d) { return function(e) { return function(f) {
	    return function(g) { return function(h) { return function(i) {
	    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
	  }
	  wrapper.arity = 9;
	  wrapper.func = fun;
	  return wrapper;
	}
	
	function A2(fun, a, b)
	{
	  return fun.arity === 2
	    ? fun.func(a, b)
	    : fun(a)(b);
	}
	function A3(fun, a, b, c)
	{
	  return fun.arity === 3
	    ? fun.func(a, b, c)
	    : fun(a)(b)(c);
	}
	function A4(fun, a, b, c, d)
	{
	  return fun.arity === 4
	    ? fun.func(a, b, c, d)
	    : fun(a)(b)(c)(d);
	}
	function A5(fun, a, b, c, d, e)
	{
	  return fun.arity === 5
	    ? fun.func(a, b, c, d, e)
	    : fun(a)(b)(c)(d)(e);
	}
	function A6(fun, a, b, c, d, e, f)
	{
	  return fun.arity === 6
	    ? fun.func(a, b, c, d, e, f)
	    : fun(a)(b)(c)(d)(e)(f);
	}
	function A7(fun, a, b, c, d, e, f, g)
	{
	  return fun.arity === 7
	    ? fun.func(a, b, c, d, e, f, g)
	    : fun(a)(b)(c)(d)(e)(f)(g);
	}
	function A8(fun, a, b, c, d, e, f, g, h)
	{
	  return fun.arity === 8
	    ? fun.func(a, b, c, d, e, f, g, h)
	    : fun(a)(b)(c)(d)(e)(f)(g)(h);
	}
	function A9(fun, a, b, c, d, e, f, g, h, i)
	{
	  return fun.arity === 9
	    ? fun.func(a, b, c, d, e, f, g, h, i)
	    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
	}
	
	//import Native.Utils //
	
	var _elm_lang$core$Native_Basics = function() {
	
	function div(a, b)
	{
		return (a / b) | 0;
	}
	function rem(a, b)
	{
		return a % b;
	}
	function mod(a, b)
	{
		if (b === 0)
		{
			throw new Error('Cannot perform mod 0. Division by zero error.');
		}
		var r = a % b;
		var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));
	
		return m === b ? 0 : m;
	}
	function logBase(base, n)
	{
		return Math.log(n) / Math.log(base);
	}
	function negate(n)
	{
		return -n;
	}
	function abs(n)
	{
		return n < 0 ? -n : n;
	}
	
	function min(a, b)
	{
		return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
	}
	function max(a, b)
	{
		return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
	}
	function clamp(lo, hi, n)
	{
		return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
			? lo
			: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
				? hi
				: n;
	}
	
	var ord = ['LT', 'EQ', 'GT'];
	
	function compare(x, y)
	{
		return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
	}
	
	function xor(a, b)
	{
		return a !== b;
	}
	function not(b)
	{
		return !b;
	}
	function isInfinite(n)
	{
		return n === Infinity || n === -Infinity;
	}
	
	function truncate(n)
	{
		return n | 0;
	}
	
	function degrees(d)
	{
		return d * Math.PI / 180;
	}
	function turns(t)
	{
		return 2 * Math.PI * t;
	}
	function fromPolar(point)
	{
		var r = point._0;
		var t = point._1;
		return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
	}
	function toPolar(point)
	{
		var x = point._0;
		var y = point._1;
		return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
	}
	
	return {
		div: F2(div),
		rem: F2(rem),
		mod: F2(mod),
	
		pi: Math.PI,
		e: Math.E,
		cos: Math.cos,
		sin: Math.sin,
		tan: Math.tan,
		acos: Math.acos,
		asin: Math.asin,
		atan: Math.atan,
		atan2: F2(Math.atan2),
	
		degrees: degrees,
		turns: turns,
		fromPolar: fromPolar,
		toPolar: toPolar,
	
		sqrt: Math.sqrt,
		logBase: F2(logBase),
		negate: negate,
		abs: abs,
		min: F2(min),
		max: F2(max),
		clamp: F3(clamp),
		compare: F2(compare),
	
		xor: F2(xor),
		not: not,
	
		truncate: truncate,
		ceiling: Math.ceil,
		floor: Math.floor,
		round: Math.round,
		toFloat: function(x) { return x; },
		isNaN: isNaN,
		isInfinite: isInfinite
	};
	
	}();
	//import //
	
	var _elm_lang$core$Native_Utils = function() {
	
	// COMPARISONS
	
	function eq(rootX, rootY)
	{
		var stack = [{ x: rootX, y: rootY }];
		while (stack.length > 0)
		{
			var front = stack.pop();
			var x = front.x;
			var y = front.y;
			if (x === y)
			{
				continue;
			}
			if (typeof x === 'object')
			{
				var c = 0;
				for (var key in x)
				{
					++c;
					if (!(key in y))
					{
						return false;
					}
					if (key === 'ctor')
					{
						continue;
					}
					stack.push({ x: x[key], y: y[key] });
				}
				if ('ctor' in x)
				{
					stack.push({ x: x.ctor, y: y.ctor});
				}
				if (c !== Object.keys(y).length)
				{
					return false;
				}
			}
			else if (typeof x === 'function')
			{
				throw new Error('Equality error: general function equality is ' +
								'undecidable, and therefore, unsupported');
			}
			else
			{
				return false;
			}
		}
		return true;
	}
	
	// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
	// the particular integer values assigned to LT, EQ, and GT.
	
	var LT = -1, EQ = 0, GT = 1;
	
	function cmp(x, y)
	{
		var ord;
		if (typeof x !== 'object')
		{
			return x === y ? EQ : x < y ? LT : GT;
		}
		else if (x instanceof String)
		{
			var a = x.valueOf();
			var b = y.valueOf();
			return a === b
				? EQ
				: a < b
					? LT
					: GT;
		}
		else if (x.ctor === '::' || x.ctor === '[]')
		{
			while (true)
			{
				if (x.ctor === '[]' && y.ctor === '[]')
				{
					return EQ;
				}
				if (x.ctor !== y.ctor)
				{
					return x.ctor === '[]' ? LT : GT;
				}
				ord = cmp(x._0, y._0);
				if (ord !== EQ)
				{
					return ord;
				}
				x = x._1;
				y = y._1;
			}
		}
		else if (x.ctor.slice(0, 6) === '_Tuple')
		{
			var n = x.ctor.slice(6) - 0;
			var err = 'cannot compare tuples with more than 6 elements.';
			if (n === 0) return EQ;
			if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
			if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
			if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
			if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
			if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
			if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
			if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
			return EQ;
		}
		else
		{
			throw new Error('Comparison error: comparison is only defined on ints, ' +
							'floats, times, chars, strings, lists of comparable values, ' +
							'and tuples of comparable values.');
		}
	}
	
	
	// COMMON VALUES
	
	var Tuple0 = {
		ctor: '_Tuple0'
	};
	
	function Tuple2(x, y)
	{
		return {
			ctor: '_Tuple2',
			_0: x,
			_1: y
		};
	}
	
	function chr(c)
	{
		return new String(c);
	}
	
	
	// GUID
	
	var count = 0;
	function guid(_)
	{
		return count++;
	}
	
	
	// RECORDS
	
	function update(oldRecord, updatedFields)
	{
		var newRecord = {};
		for (var key in oldRecord)
		{
			var value = (key in updatedFields) ? updatedFields[key] : oldRecord[key];
			newRecord[key] = value;
		}
		return newRecord;
	}
	
	
	//// LIST STUFF ////
	
	var Nil = { ctor: '[]' };
	
	function Cons(hd, tl)
	{
		return {
			ctor: '::',
			_0: hd,
			_1: tl
		};
	}
	
	function append(xs, ys)
	{
		// append Strings
		if (typeof xs === 'string')
		{
			return xs + ys;
		}
	
		// append Lists
		if (xs.ctor === '[]')
		{
			return ys;
		}
		var root = Cons(xs._0, Nil);
		var curr = root;
		xs = xs._1;
		while (xs.ctor !== '[]')
		{
			curr._1 = Cons(xs._0, Nil);
			xs = xs._1;
			curr = curr._1;
		}
		curr._1 = ys;
		return root;
	}
	
	
	// CRASHES
	
	function crash(moduleName, region)
	{
		return function(message) {
			throw new Error(
				'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
				+ 'The message provided by the code author is:\n\n    '
				+ message
			);
		};
	}
	
	function crashCase(moduleName, region, value)
	{
		return function(message) {
			throw new Error(
				'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
				+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
				+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
				+ 'The message provided by the code author is:\n\n    '
				+ message
			);
		};
	}
	
	function regionToString(region)
	{
		if (region.start.line == region.end.line)
		{
			return 'on line ' + region.start.line;
		}
		return 'between lines ' + region.start.line + ' and ' + region.end.line;
	}
	
	
	// TO STRING
	
	function toString(v)
	{
		var type = typeof v;
		if (type === 'function')
		{
			var name = v.func ? v.func.name : v.name;
			return '<function' + (name === '' ? '' : ':') + name + '>';
		}
	
		if (type === 'boolean')
		{
			return v ? 'True' : 'False';
		}
	
		if (type === 'number')
		{
			return v + '';
		}
	
		if (v instanceof String)
		{
			return '\'' + addSlashes(v, true) + '\'';
		}
	
		if (type === 'string')
		{
			return '"' + addSlashes(v, false) + '"';
		}
	
		if (v === null)
		{
			return 'null';
		}
	
		if (type === 'object' && 'ctor' in v)
		{
			var ctorStarter = v.ctor.substring(0, 5);
	
			if (ctorStarter === '_Tupl')
			{
				var output = [];
				for (var k in v)
				{
					if (k === 'ctor') continue;
					output.push(toString(v[k]));
				}
				return '(' + output.join(',') + ')';
			}
	
			if (ctorStarter === '_Task')
			{
				return '<task>'
			}
	
			if (v.ctor === '_Array')
			{
				var list = _elm_lang$core$Array$toList(v);
				return 'Array.fromList ' + toString(list);
			}
	
			if (v.ctor === '<decoder>')
			{
				return '<decoder>';
			}
	
			if (v.ctor === '_Process')
			{
				return '<process:' + v.id + '>';
			}
	
			if (v.ctor === '::')
			{
				var output = '[' + toString(v._0);
				v = v._1;
				while (v.ctor === '::')
				{
					output += ',' + toString(v._0);
					v = v._1;
				}
				return output + ']';
			}
	
			if (v.ctor === '[]')
			{
				return '[]';
			}
	
			if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin' || v.ctor === 'Set_elm_builtin')
			{
				var name, list;
				if (v.ctor === 'Set_elm_builtin')
				{
					name = 'Set';
					list = A2(
						_elm_lang$core$List$map,
						function(x) {return x._0; },
						_elm_lang$core$Dict$toList(v._0)
					);
				}
				else
				{
					name = 'Dict';
					list = _elm_lang$core$Dict$toList(v);
				}
				return name + '.fromList ' + toString(list);
			}
	
			var output = '';
			for (var i in v)
			{
				if (i === 'ctor') continue;
				var str = toString(v[i]);
				var c0 = str[0];
				var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
				output += ' ' + (parenless ? str : '(' + str + ')');
			}
			return v.ctor + output;
		}
	
		if (type === 'object')
		{
			var output = [];
			for (var k in v)
			{
				output.push(k + ' = ' + toString(v[k]));
			}
			if (output.length === 0)
			{
				return '{}';
			}
			return '{ ' + output.join(', ') + ' }';
		}
	
		return '<internal structure>';
	}
	
	function addSlashes(str, isChar)
	{
		var s = str.replace(/\\/g, '\\\\')
				  .replace(/\n/g, '\\n')
				  .replace(/\t/g, '\\t')
				  .replace(/\r/g, '\\r')
				  .replace(/\v/g, '\\v')
				  .replace(/\0/g, '\\0');
		if (isChar)
		{
			return s.replace(/\'/g, '\\\'');
		}
		else
		{
			return s.replace(/\"/g, '\\"');
		}
	}
	
	
	return {
		eq: eq,
		cmp: cmp,
		Tuple0: Tuple0,
		Tuple2: Tuple2,
		chr: chr,
		update: update,
		guid: guid,
	
		append: F2(append),
	
		crash: crash,
		crashCase: crashCase,
	
		toString: toString
	};
	
	}();
	var _elm_lang$core$Basics$uncurry = F2(
		function (f, _p0) {
			var _p1 = _p0;
			return A2(f, _p1._0, _p1._1);
		});
	var _elm_lang$core$Basics$curry = F3(
		function (f, a, b) {
			return f(
				{ctor: '_Tuple2', _0: a, _1: b});
		});
	var _elm_lang$core$Basics$flip = F3(
		function (f, b, a) {
			return A2(f, a, b);
		});
	var _elm_lang$core$Basics$snd = function (_p2) {
		var _p3 = _p2;
		return _p3._1;
	};
	var _elm_lang$core$Basics$fst = function (_p4) {
		var _p5 = _p4;
		return _p5._0;
	};
	var _elm_lang$core$Basics$always = F2(
		function (a, _p6) {
			return a;
		});
	var _elm_lang$core$Basics$identity = function (x) {
		return x;
	};
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['<|'] = F2(
		function (f, x) {
			return f(x);
		});
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['|>'] = F2(
		function (x, f) {
			return f(x);
		});
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['>>'] = F3(
		function (f, g, x) {
			return g(
				f(x));
		});
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['<<'] = F3(
		function (g, f, x) {
			return g(
				f(x));
		});
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
	var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
	var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
	var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
	var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
	var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
	var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
	var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
	var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
	var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
	var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
	var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
	var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
	var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
	var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
	var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
	var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
	var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
	var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
	var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
	var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
	var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
	var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
	var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
	var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
	var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
	var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
	var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
	var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
	var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
	_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
	var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
	var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
	var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
	var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
	var _elm_lang$core$Basics$radians = function (t) {
		return t;
	};
	var _elm_lang$core$Basics$GT = {ctor: 'GT'};
	var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
	var _elm_lang$core$Basics$LT = {ctor: 'LT'};
	var _elm_lang$core$Basics$Never = function (a) {
		return {ctor: 'Never', _0: a};
	};
	
	//import Native.Utils //
	
	var _elm_lang$core$Native_Debug = function() {
	
	function log(tag, value)
	{
		var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
		var process = process || {};
		if (process.stdout)
		{
			process.stdout.write(msg);
		}
		else
		{
			console.log(msg);
		}
		return value;
	}
	
	function crash(message)
	{
		throw new Error(message);
	}
	
	return {
		crash: crash,
		log: F2(log)
	};
	
	}();
	var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
	var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;
	
	var _elm_lang$core$Maybe$withDefault = F2(
		function ($default, maybe) {
			var _p0 = maybe;
			if (_p0.ctor === 'Just') {
				return _p0._0;
			} else {
				return $default;
			}
		});
	var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
	var _elm_lang$core$Maybe$oneOf = function (maybes) {
		oneOf:
		while (true) {
			var _p1 = maybes;
			if (_p1.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p3 = _p1._0;
				var _p2 = _p3;
				if (_p2.ctor === 'Nothing') {
					var _v3 = _p1._1;
					maybes = _v3;
					continue oneOf;
				} else {
					return _p3;
				}
			}
		}
	};
	var _elm_lang$core$Maybe$andThen = F2(
		function (maybeValue, callback) {
			var _p4 = maybeValue;
			if (_p4.ctor === 'Just') {
				return callback(_p4._0);
			} else {
				return _elm_lang$core$Maybe$Nothing;
			}
		});
	var _elm_lang$core$Maybe$Just = function (a) {
		return {ctor: 'Just', _0: a};
	};
	var _elm_lang$core$Maybe$map = F2(
		function (f, maybe) {
			var _p5 = maybe;
			if (_p5.ctor === 'Just') {
				return _elm_lang$core$Maybe$Just(
					f(_p5._0));
			} else {
				return _elm_lang$core$Maybe$Nothing;
			}
		});
	var _elm_lang$core$Maybe$map2 = F3(
		function (func, ma, mb) {
			var _p6 = {ctor: '_Tuple2', _0: ma, _1: mb};
			if (((_p6.ctor === '_Tuple2') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) {
				return _elm_lang$core$Maybe$Just(
					A2(func, _p6._0._0, _p6._1._0));
			} else {
				return _elm_lang$core$Maybe$Nothing;
			}
		});
	var _elm_lang$core$Maybe$map3 = F4(
		function (func, ma, mb, mc) {
			var _p7 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
			if ((((_p7.ctor === '_Tuple3') && (_p7._0.ctor === 'Just')) && (_p7._1.ctor === 'Just')) && (_p7._2.ctor === 'Just')) {
				return _elm_lang$core$Maybe$Just(
					A3(func, _p7._0._0, _p7._1._0, _p7._2._0));
			} else {
				return _elm_lang$core$Maybe$Nothing;
			}
		});
	var _elm_lang$core$Maybe$map4 = F5(
		function (func, ma, mb, mc, md) {
			var _p8 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
			if (((((_p8.ctor === '_Tuple4') && (_p8._0.ctor === 'Just')) && (_p8._1.ctor === 'Just')) && (_p8._2.ctor === 'Just')) && (_p8._3.ctor === 'Just')) {
				return _elm_lang$core$Maybe$Just(
					A4(func, _p8._0._0, _p8._1._0, _p8._2._0, _p8._3._0));
			} else {
				return _elm_lang$core$Maybe$Nothing;
			}
		});
	var _elm_lang$core$Maybe$map5 = F6(
		function (func, ma, mb, mc, md, me) {
			var _p9 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
			if ((((((_p9.ctor === '_Tuple5') && (_p9._0.ctor === 'Just')) && (_p9._1.ctor === 'Just')) && (_p9._2.ctor === 'Just')) && (_p9._3.ctor === 'Just')) && (_p9._4.ctor === 'Just')) {
				return _elm_lang$core$Maybe$Just(
					A5(func, _p9._0._0, _p9._1._0, _p9._2._0, _p9._3._0, _p9._4._0));
			} else {
				return _elm_lang$core$Maybe$Nothing;
			}
		});
	
	//import Native.Utils //
	
	var _elm_lang$core$Native_List = function() {
	
	var Nil = { ctor: '[]' };
	
	function Cons(hd, tl)
	{
		return { ctor: '::', _0: hd, _1: tl };
	}
	
	function fromArray(arr)
	{
		var out = Nil;
		for (var i = arr.length; i--; )
		{
			out = Cons(arr[i], out);
		}
		return out;
	}
	
	function toArray(xs)
	{
		var out = [];
		while (xs.ctor !== '[]')
		{
			out.push(xs._0);
			xs = xs._1;
		}
		return out;
	}
	
	
	function range(lo, hi)
	{
		var list = Nil;
		if (lo <= hi)
		{
			do
			{
				list = Cons(hi, list);
			}
			while (hi-- > lo);
		}
		return list;
	}
	
	function foldr(f, b, xs)
	{
		var arr = toArray(xs);
		var acc = b;
		for (var i = arr.length; i--; )
		{
			acc = A2(f, arr[i], acc);
		}
		return acc;
	}
	
	function map2(f, xs, ys)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]')
		{
			arr.push(A2(f, xs._0, ys._0));
			xs = xs._1;
			ys = ys._1;
		}
		return fromArray(arr);
	}
	
	function map3(f, xs, ys, zs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
		{
			arr.push(A3(f, xs._0, ys._0, zs._0));
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}
	
	function map4(f, ws, xs, ys, zs)
	{
		var arr = [];
		while (   ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}
	
	function map5(f, vs, ws, xs, ys, zs)
	{
		var arr = [];
		while (   vs.ctor !== '[]'
			   && ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
			vs = vs._1;
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}
	
	function sortBy(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a, b) {
			return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
		}));
	}
	
	function sortWith(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a, b) {
			var ord = f(a)(b).ctor;
			return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
		}));
	}
	
	return {
		Nil: Nil,
		Cons: Cons,
		cons: F2(Cons),
		toArray: toArray,
		fromArray: fromArray,
		range: range,
	
		foldr: F3(foldr),
	
		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		sortBy: F2(sortBy),
		sortWith: F2(sortWith)
	};
	
	}();
	var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
	var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
	var _elm_lang$core$List$sort = function (xs) {
		return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
	};
	var _elm_lang$core$List$drop = F2(
		function (n, list) {
			drop:
			while (true) {
				if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
					return list;
				} else {
					var _p0 = list;
					if (_p0.ctor === '[]') {
						return list;
					} else {
						var _v1 = n - 1,
							_v2 = _p0._1;
						n = _v1;
						list = _v2;
						continue drop;
					}
				}
			}
		});
	var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
	var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
	var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
	var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
	var _elm_lang$core$List$any = F2(
		function (isOkay, list) {
			any:
			while (true) {
				var _p1 = list;
				if (_p1.ctor === '[]') {
					return false;
				} else {
					if (isOkay(_p1._0)) {
						return true;
					} else {
						var _v4 = isOkay,
							_v5 = _p1._1;
						isOkay = _v4;
						list = _v5;
						continue any;
					}
				}
			}
		});
	var _elm_lang$core$List$all = F2(
		function (isOkay, list) {
			return _elm_lang$core$Basics$not(
				A2(
					_elm_lang$core$List$any,
					function (_p2) {
						return _elm_lang$core$Basics$not(
							isOkay(_p2));
					},
					list));
		});
	var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
	var _elm_lang$core$List$foldl = F3(
		function (func, acc, list) {
			foldl:
			while (true) {
				var _p3 = list;
				if (_p3.ctor === '[]') {
					return acc;
				} else {
					var _v7 = func,
						_v8 = A2(func, _p3._0, acc),
						_v9 = _p3._1;
					func = _v7;
					acc = _v8;
					list = _v9;
					continue foldl;
				}
			}
		});
	var _elm_lang$core$List$length = function (xs) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p4, i) {
					return i + 1;
				}),
			0,
			xs);
	};
	var _elm_lang$core$List$sum = function (numbers) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, y) {
					return x + y;
				}),
			0,
			numbers);
	};
	var _elm_lang$core$List$product = function (numbers) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, y) {
					return x * y;
				}),
			1,
			numbers);
	};
	var _elm_lang$core$List$maximum = function (list) {
		var _p5 = list;
		if (_p5.ctor === '::') {
			return _elm_lang$core$Maybe$Just(
				A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	};
	var _elm_lang$core$List$minimum = function (list) {
		var _p6 = list;
		if (_p6.ctor === '::') {
			return _elm_lang$core$Maybe$Just(
				A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	};
	var _elm_lang$core$List$indexedMap = F2(
		function (f, xs) {
			return A3(
				_elm_lang$core$List$map2,
				f,
				_elm_lang$core$Native_List.range(
					0,
					_elm_lang$core$List$length(xs) - 1),
				xs);
		});
	var _elm_lang$core$List$member = F2(
		function (x, xs) {
			return A2(
				_elm_lang$core$List$any,
				function (a) {
					return _elm_lang$core$Native_Utils.eq(a, x);
				},
				xs);
		});
	var _elm_lang$core$List$isEmpty = function (xs) {
		var _p7 = xs;
		if (_p7.ctor === '[]') {
			return true;
		} else {
			return false;
		}
	};
	var _elm_lang$core$List$tail = function (list) {
		var _p8 = list;
		if (_p8.ctor === '::') {
			return _elm_lang$core$Maybe$Just(_p8._1);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	};
	var _elm_lang$core$List$head = function (list) {
		var _p9 = list;
		if (_p9.ctor === '::') {
			return _elm_lang$core$Maybe$Just(_p9._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	};
	var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
	_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
	var _elm_lang$core$List$map = F2(
		function (f, xs) {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, acc) {
						return A2(
							_elm_lang$core$List_ops['::'],
							f(x),
							acc);
					}),
				_elm_lang$core$Native_List.fromArray(
					[]),
				xs);
		});
	var _elm_lang$core$List$filter = F2(
		function (pred, xs) {
			var conditionalCons = F2(
				function (x, xs$) {
					return pred(x) ? A2(_elm_lang$core$List_ops['::'], x, xs$) : xs$;
				});
			return A3(
				_elm_lang$core$List$foldr,
				conditionalCons,
				_elm_lang$core$Native_List.fromArray(
					[]),
				xs);
		});
	var _elm_lang$core$List$maybeCons = F3(
		function (f, mx, xs) {
			var _p10 = f(mx);
			if (_p10.ctor === 'Just') {
				return A2(_elm_lang$core$List_ops['::'], _p10._0, xs);
			} else {
				return xs;
			}
		});
	var _elm_lang$core$List$filterMap = F2(
		function (f, xs) {
			return A3(
				_elm_lang$core$List$foldr,
				_elm_lang$core$List$maybeCons(f),
				_elm_lang$core$Native_List.fromArray(
					[]),
				xs);
		});
	var _elm_lang$core$List$reverse = function (list) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, y) {
					return A2(_elm_lang$core$List_ops['::'], x, y);
				}),
			_elm_lang$core$Native_List.fromArray(
				[]),
			list);
	};
	var _elm_lang$core$List$scanl = F3(
		function (f, b, xs) {
			var scan1 = F2(
				function (x, accAcc) {
					var _p11 = accAcc;
					if (_p11.ctor === '::') {
						return A2(
							_elm_lang$core$List_ops['::'],
							A2(f, x, _p11._0),
							accAcc);
					} else {
						return _elm_lang$core$Native_List.fromArray(
							[]);
					}
				});
			return _elm_lang$core$List$reverse(
				A3(
					_elm_lang$core$List$foldl,
					scan1,
					_elm_lang$core$Native_List.fromArray(
						[b]),
					xs));
		});
	var _elm_lang$core$List$append = F2(
		function (xs, ys) {
			var _p12 = ys;
			if (_p12.ctor === '[]') {
				return xs;
			} else {
				return A3(
					_elm_lang$core$List$foldr,
					F2(
						function (x, y) {
							return A2(_elm_lang$core$List_ops['::'], x, y);
						}),
					ys,
					xs);
			}
		});
	var _elm_lang$core$List$concat = function (lists) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$append,
			_elm_lang$core$Native_List.fromArray(
				[]),
			lists);
	};
	var _elm_lang$core$List$concatMap = F2(
		function (f, list) {
			return _elm_lang$core$List$concat(
				A2(_elm_lang$core$List$map, f, list));
		});
	var _elm_lang$core$List$partition = F2(
		function (pred, list) {
			var step = F2(
				function (x, _p13) {
					var _p14 = _p13;
					var _p16 = _p14._0;
					var _p15 = _p14._1;
					return pred(x) ? {
						ctor: '_Tuple2',
						_0: A2(_elm_lang$core$List_ops['::'], x, _p16),
						_1: _p15
					} : {
						ctor: '_Tuple2',
						_0: _p16,
						_1: A2(_elm_lang$core$List_ops['::'], x, _p15)
					};
				});
			return A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_List.fromArray(
						[]),
					_1: _elm_lang$core$Native_List.fromArray(
						[])
				},
				list);
		});
	var _elm_lang$core$List$unzip = function (pairs) {
		var step = F2(
			function (_p18, _p17) {
				var _p19 = _p18;
				var _p20 = _p17;
				return {
					ctor: '_Tuple2',
					_0: A2(_elm_lang$core$List_ops['::'], _p19._0, _p20._0),
					_1: A2(_elm_lang$core$List_ops['::'], _p19._1, _p20._1)
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_List.fromArray(
					[]),
				_1: _elm_lang$core$Native_List.fromArray(
					[])
			},
			pairs);
	};
	var _elm_lang$core$List$intersperse = F2(
		function (sep, xs) {
			var _p21 = xs;
			if (_p21.ctor === '[]') {
				return _elm_lang$core$Native_List.fromArray(
					[]);
			} else {
				var step = F2(
					function (x, rest) {
						return A2(
							_elm_lang$core$List_ops['::'],
							sep,
							A2(_elm_lang$core$List_ops['::'], x, rest));
					});
				var spersed = A3(
					_elm_lang$core$List$foldr,
					step,
					_elm_lang$core$Native_List.fromArray(
						[]),
					_p21._1);
				return A2(_elm_lang$core$List_ops['::'], _p21._0, spersed);
			}
		});
	var _elm_lang$core$List$take = F2(
		function (n, list) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return _elm_lang$core$Native_List.fromArray(
					[]);
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return list;
				} else {
					return A2(
						_elm_lang$core$List_ops['::'],
						_p22._0,
						A2(_elm_lang$core$List$take, n - 1, _p22._1));
				}
			}
		});
	var _elm_lang$core$List$repeatHelp = F3(
		function (result, n, value) {
			repeatHelp:
			while (true) {
				if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
					return result;
				} else {
					var _v23 = A2(_elm_lang$core$List_ops['::'], value, result),
						_v24 = n - 1,
						_v25 = value;
					result = _v23;
					n = _v24;
					value = _v25;
					continue repeatHelp;
				}
			}
		});
	var _elm_lang$core$List$repeat = F2(
		function (n, value) {
			return A3(
				_elm_lang$core$List$repeatHelp,
				_elm_lang$core$Native_List.fromArray(
					[]),
				n,
				value);
		});
	
	var _elm_lang$core$Result$toMaybe = function (result) {
		var _p0 = result;
		if (_p0.ctor === 'Ok') {
			return _elm_lang$core$Maybe$Just(_p0._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	};
	var _elm_lang$core$Result$withDefault = F2(
		function (def, result) {
			var _p1 = result;
			if (_p1.ctor === 'Ok') {
				return _p1._0;
			} else {
				return def;
			}
		});
	var _elm_lang$core$Result$Err = function (a) {
		return {ctor: 'Err', _0: a};
	};
	var _elm_lang$core$Result$andThen = F2(
		function (result, callback) {
			var _p2 = result;
			if (_p2.ctor === 'Ok') {
				return callback(_p2._0);
			} else {
				return _elm_lang$core$Result$Err(_p2._0);
			}
		});
	var _elm_lang$core$Result$Ok = function (a) {
		return {ctor: 'Ok', _0: a};
	};
	var _elm_lang$core$Result$map = F2(
		function (func, ra) {
			var _p3 = ra;
			if (_p3.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					func(_p3._0));
			} else {
				return _elm_lang$core$Result$Err(_p3._0);
			}
		});
	var _elm_lang$core$Result$map2 = F3(
		function (func, ra, rb) {
			var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
			if (_p4._0.ctor === 'Ok') {
				if (_p4._1.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A2(func, _p4._0._0, _p4._1._0));
				} else {
					return _elm_lang$core$Result$Err(_p4._1._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p4._0._0);
			}
		});
	var _elm_lang$core$Result$map3 = F4(
		function (func, ra, rb, rc) {
			var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
			if (_p5._0.ctor === 'Ok') {
				if (_p5._1.ctor === 'Ok') {
					if (_p5._2.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
					} else {
						return _elm_lang$core$Result$Err(_p5._2._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p5._1._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._0._0);
			}
		});
	var _elm_lang$core$Result$map4 = F5(
		function (func, ra, rb, rc, rd) {
			var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
			if (_p6._0.ctor === 'Ok') {
				if (_p6._1.ctor === 'Ok') {
					if (_p6._2.ctor === 'Ok') {
						if (_p6._3.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
						} else {
							return _elm_lang$core$Result$Err(_p6._3._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p6._2._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._1._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._0._0);
			}
		});
	var _elm_lang$core$Result$map5 = F6(
		function (func, ra, rb, rc, rd, re) {
			var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
			if (_p7._0.ctor === 'Ok') {
				if (_p7._1.ctor === 'Ok') {
					if (_p7._2.ctor === 'Ok') {
						if (_p7._3.ctor === 'Ok') {
							if (_p7._4.ctor === 'Ok') {
								return _elm_lang$core$Result$Ok(
									A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
							} else {
								return _elm_lang$core$Result$Err(_p7._4._0);
							}
						} else {
							return _elm_lang$core$Result$Err(_p7._3._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._2._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._1._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._0._0);
			}
		});
	var _elm_lang$core$Result$formatError = F2(
		function (f, result) {
			var _p8 = result;
			if (_p8.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(_p8._0);
			} else {
				return _elm_lang$core$Result$Err(
					f(_p8._0));
			}
		});
	var _elm_lang$core$Result$fromMaybe = F2(
		function (err, maybe) {
			var _p9 = maybe;
			if (_p9.ctor === 'Just') {
				return _elm_lang$core$Result$Ok(_p9._0);
			} else {
				return _elm_lang$core$Result$Err(err);
			}
		});
	
	//import //
	
	var _elm_lang$core$Native_Platform = function() {
	
	
	// PROGRAMS
	
	function addPublicModule(object, name, main)
	{
		var init = main ? makeEmbed(name, main) : mainIsUndefined(name);
	
		object['worker'] = function worker(flags)
		{
			return init(undefined, flags, false);
		}
	
		object['embed'] = function embed(domNode, flags)
		{
			return init(domNode, flags, true);
		}
	
		object['fullscreen'] = function fullscreen(flags)
		{
			return init(document.body, flags, true);
		};
	}
	
	
	// PROGRAM FAIL
	
	function mainIsUndefined(name)
	{
		return function(domNode)
		{
			var message = 'Cannot initialize module `' + name +
				'` because it has no `main` value!\nWhat should I show on screen?';
			domNode.innerHTML = errorHtml(message);
			throw new Error(message);
		};
	}
	
	function errorHtml(message)
	{
		return '<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + message + '</pre>'
			+ '</div>';
	}
	
	
	// PROGRAM SUCCESS
	
	function makeEmbed(moduleName, main)
	{
		return function embed(rootDomNode, flags, withRenderer)
		{
			try
			{
				var program = mainToProgram(moduleName, main);
				if (!withRenderer)
				{
					program.renderer = dummyRenderer;
				}
				return makeEmbedHelp(moduleName, program, rootDomNode, flags);
			}
			catch (e)
			{
				rootDomNode.innerHTML = errorHtml(e.message);
				throw e;
			}
		};
	}
	
	function dummyRenderer()
	{
		return { update: function() {} };
	}
	
	
	// MAIN TO PROGRAM
	
	function mainToProgram(moduleName, wrappedMain)
	{
		var main = wrappedMain.main;
	
		if (typeof main.init === 'undefined')
		{
			var emptyBag = batch(_elm_lang$core$Native_List.Nil);
			var noChange = _elm_lang$core$Native_Utils.Tuple2(
				_elm_lang$core$Native_Utils.Tuple0,
				emptyBag
			);
	
			return _elm_lang$virtual_dom$VirtualDom$programWithFlags({
				init: function() { return noChange; },
				view: function() { return main; },
				update: F2(function() { return noChange; }),
				subscriptions: function () { return emptyBag; }
			});
		}
	
		var flags = wrappedMain.flags;
		var init = flags
			? initWithFlags(moduleName, main.init, flags)
			: initWithoutFlags(moduleName, main.init);
	
		return _elm_lang$virtual_dom$VirtualDom$programWithFlags({
			init: init,
			view: main.view,
			update: main.update,
			subscriptions: main.subscriptions,
		});
	}
	
	function initWithoutFlags(moduleName, realInit)
	{
		return function init(flags)
		{
			if (typeof flags !== 'undefined')
			{
				throw new Error(
					'You are giving module `' + moduleName + '` an argument in JavaScript.\n'
					+ 'This module does not take arguments though! You probably need to change the\n'
					+ 'initialization code to something like `Elm.' + moduleName + '.fullscreen()`'
				);
			}
			return realInit();
		};
	}
	
	function initWithFlags(moduleName, realInit, flagDecoder)
	{
		return function init(flags)
		{
			var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
			if (result.ctor === 'Err')
			{
				throw new Error(
					'You are trying to initialize module `' + moduleName + '` with an unexpected argument.\n'
					+ 'When trying to convert it to a usable Elm value, I run into this problem:\n\n'
					+ result._0
				);
			}
			return realInit(result._0);
		};
	}
	
	
	// SETUP RUNTIME SYSTEM
	
	function makeEmbedHelp(moduleName, program, rootDomNode, flags)
	{
		var init = program.init;
		var update = program.update;
		var subscriptions = program.subscriptions;
		var view = program.view;
		var makeRenderer = program.renderer;
	
		// ambient state
		var managers = {};
		var renderer;
	
		// init and update state in main process
		var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = init(flags);
			var model = results._0;
			renderer = makeRenderer(rootDomNode, enqueue, view(model));
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	
		function onMessage(msg, model)
		{
			return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
				var results = A2(update, msg, model);
				model = results._0;
				renderer.update(view(model));
				var cmds = results._1;
				var subs = subscriptions(model);
				dispatchEffects(managers, cmds, subs);
				callback(_elm_lang$core$Native_Scheduler.succeed(model));
			});
		}
	
		var mainProcess = spawnLoop(initApp, onMessage);
	
		function enqueue(msg)
		{
			_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
		}
	
		var ports = setupEffects(managers, enqueue);
	
		return ports ? { ports: ports } : {};
	}
	
	
	// EFFECT MANAGERS
	
	var effectManagers = {};
	
	function setupEffects(managers, callback)
	{
		var ports;
	
		// setup all necessary effect managers
		for (var key in effectManagers)
		{
			var manager = effectManagers[key];
	
			if (manager.isForeign)
			{
				ports = ports || {};
				ports[key] = manager.tag === 'cmd'
					? setupOutgoingPort(key)
					: setupIncomingPort(key, callback);
			}
	
			managers[key] = makeManager(manager, callback);
		}
	
		return ports;
	}
	
	function makeManager(info, callback)
	{
		var router = {
			main: callback,
			self: undefined
		};
	
		var tag = info.tag;
		var onEffects = info.onEffects;
		var onSelfMsg = info.onSelfMsg;
	
		function onMessage(msg, state)
		{
			if (msg.ctor === 'self')
			{
				return A3(onSelfMsg, router, msg._0, state);
			}
	
			var fx = msg._0;
			switch (tag)
			{
				case 'cmd':
					return A3(onEffects, router, fx.cmds, state);
	
				case 'sub':
					return A3(onEffects, router, fx.subs, state);
	
				case 'fx':
					return A4(onEffects, router, fx.cmds, fx.subs, state);
			}
		}
	
		var process = spawnLoop(info.init, onMessage);
		router.self = process;
		return process;
	}
	
	function sendToApp(router, msg)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
		{
			router.main(msg);
			callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
		});
	}
	
	function sendToSelf(router, msg)
	{
		return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
			ctor: 'self',
			_0: msg
		});
	}
	
	
	// HELPER for STATEFUL LOOPS
	
	function spawnLoop(init, onMessage)
	{
		var andThen = _elm_lang$core$Native_Scheduler.andThen;
	
		function loop(state)
		{
			var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
				return onMessage(msg, state);
			});
			return A2(andThen, handleMsg, loop);
		}
	
		var task = A2(andThen, init, loop);
	
		return _elm_lang$core$Native_Scheduler.rawSpawn(task);
	}
	
	
	// BAGS
	
	function leaf(home)
	{
		return function(value)
		{
			return {
				type: 'leaf',
				home: home,
				value: value
			};
		};
	}
	
	function batch(list)
	{
		return {
			type: 'node',
			branches: list
		};
	}
	
	function map(tagger, bag)
	{
		return {
			type: 'map',
			tagger: tagger,
			tree: bag
		}
	}
	
	
	// PIPE BAGS INTO EFFECT MANAGERS
	
	function dispatchEffects(managers, cmdBag, subBag)
	{
		var effectsDict = {};
		gatherEffects(true, cmdBag, effectsDict, null);
		gatherEffects(false, subBag, effectsDict, null);
	
		for (var home in managers)
		{
			var fx = home in effectsDict
				? effectsDict[home]
				: {
					cmds: _elm_lang$core$Native_List.Nil,
					subs: _elm_lang$core$Native_List.Nil
				};
	
			_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
		}
	}
	
	function gatherEffects(isCmd, bag, effectsDict, taggers)
	{
		switch (bag.type)
		{
			case 'leaf':
				var home = bag.home;
				var effect = toEffect(isCmd, home, taggers, bag.value);
				effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
				return;
	
			case 'node':
				var list = bag.branches;
				while (list.ctor !== '[]')
				{
					gatherEffects(isCmd, list._0, effectsDict, taggers);
					list = list._1;
				}
				return;
	
			case 'map':
				gatherEffects(isCmd, bag.tree, effectsDict, {
					tagger: bag.tagger,
					rest: taggers
				});
				return;
		}
	}
	
	function toEffect(isCmd, home, taggers, value)
	{
		function applyTaggers(x)
		{
			var temp = taggers;
			while (temp)
			{
				x = temp.tagger(x);
				temp = temp.rest;
			}
			return x;
		}
	
		var map = isCmd
			? effectManagers[home].cmdMap
			: effectManagers[home].subMap;
	
		return A2(map, applyTaggers, value)
	}
	
	function insert(isCmd, newEffect, effects)
	{
		effects = effects || {
			cmds: _elm_lang$core$Native_List.Nil,
			subs: _elm_lang$core$Native_List.Nil
		};
		if (isCmd)
		{
			effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
			return effects;
		}
		effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
		return effects;
	}
	
	
	// PORTS
	
	function checkPortName(name)
	{
		if (name in effectManagers)
		{
			throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
		}
	}
	
	
	// OUTGOING PORTS
	
	function outgoingPort(name, converter)
	{
		checkPortName(name);
		effectManagers[name] = {
			tag: 'cmd',
			cmdMap: outgoingPortMap,
			converter: converter,
			isForeign: true
		};
		return leaf(name);
	}
	
	var outgoingPortMap = F2(function cmdMap(tagger, value) {
		return value;
	});
	
	function setupOutgoingPort(name)
	{
		var subs = [];
		var converter = effectManagers[name].converter;
	
		// CREATE MANAGER
	
		var init = _elm_lang$core$Native_Scheduler.succeed(null);
	
		function onEffects(router, cmdList, state)
		{
			while (cmdList.ctor !== '[]')
			{
				var value = converter(cmdList._0);
				for (var i = 0; i < subs.length; i++)
				{
					subs[i](value);
				}
				cmdList = cmdList._1;
			}
			return init;
		}
	
		effectManagers[name].init = init;
		effectManagers[name].onEffects = F3(onEffects);
	
		// PUBLIC API
	
		function subscribe(callback)
		{
			subs.push(callback);
		}
	
		function unsubscribe(callback)
		{
			var index = subs.indexOf(callback);
			if (index >= 0)
			{
				subs.splice(index, 1);
			}
		}
	
		return {
			subscribe: subscribe,
			unsubscribe: unsubscribe
		};
	}
	
	
	// INCOMING PORTS
	
	function incomingPort(name, converter)
	{
		checkPortName(name);
		effectManagers[name] = {
			tag: 'sub',
			subMap: incomingPortMap,
			converter: converter,
			isForeign: true
		};
		return leaf(name);
	}
	
	var incomingPortMap = F2(function subMap(tagger, finalTagger)
	{
		return function(value)
		{
			return tagger(finalTagger(value));
		};
	});
	
	function setupIncomingPort(name, callback)
	{
		var subs = _elm_lang$core$Native_List.Nil;
		var converter = effectManagers[name].converter;
	
		// CREATE MANAGER
	
		var init = _elm_lang$core$Native_Scheduler.succeed(null);
	
		function onEffects(router, subList, state)
		{
			subs = subList;
			return init;
		}
	
		effectManagers[name].init = init;
		effectManagers[name].onEffects = F3(onEffects);
	
		// PUBLIC API
	
		function send(value)
		{
			var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, value);
			if (result.ctor === 'Err')
			{
				throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
			}
	
			var value = result._0;
			var temp = subs;
			while (temp.ctor !== '[]')
			{
				callback(temp._0(value));
				temp = temp._1;
			}
		}
	
		return { send: send };
	}
	
	return {
		// routers
		sendToApp: F2(sendToApp),
		sendToSelf: F2(sendToSelf),
	
		// global setup
		mainToProgram: mainToProgram,
		effectManagers: effectManagers,
		outgoingPort: outgoingPort,
		incomingPort: incomingPort,
		addPublicModule: addPublicModule,
	
		// effect bags
		leaf: leaf,
		batch: batch,
		map: F2(map)
	};
	
	}();
	//import Native.Utils //
	
	var _elm_lang$core$Native_Scheduler = function() {
	
	var MAX_STEPS = 10000;
	
	
	// TASKS
	
	function succeed(value)
	{
		return {
			ctor: '_Task_succeed',
			value: value
		};
	}
	
	function fail(error)
	{
		return {
			ctor: '_Task_fail',
			value: error
		};
	}
	
	function nativeBinding(callback)
	{
		return {
			ctor: '_Task_nativeBinding',
			callback: callback,
			cancel: null
		};
	}
	
	function andThen(task, callback)
	{
		return {
			ctor: '_Task_andThen',
			task: task,
			callback: callback
		};
	}
	
	function onError(task, callback)
	{
		return {
			ctor: '_Task_onError',
			task: task,
			callback: callback
		};
	}
	
	function receive(callback)
	{
		return {
			ctor: '_Task_receive',
			callback: callback
		};
	}
	
	
	// PROCESSES
	
	function rawSpawn(task)
	{
		var process = {
			ctor: '_Process',
			id: _elm_lang$core$Native_Utils.guid(),
			root: task,
			stack: null,
			mailbox: []
		};
	
		enqueue(process);
	
		return process;
	}
	
	function spawn(task)
	{
		return nativeBinding(function(callback) {
			var process = rawSpawn(task);
			callback(succeed(process));
		});
	}
	
	function rawSend(process, msg)
	{
		process.mailbox.push(msg);
		enqueue(process);
	}
	
	function send(process, msg)
	{
		return nativeBinding(function(callback) {
			rawSend(process, msg);
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		});
	}
	
	function kill(process)
	{
		return nativeBinding(function(callback) {
			var root = process.root;
			if (root.ctor === '_Task_nativeBinding' && root.cancel)
			{
				root.cancel();
			}
	
			process.root = null;
	
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		});
	}
	
	function sleep(time)
	{
		return nativeBinding(function(callback) {
			var id = setTimeout(function() {
				callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
			}, time);
	
			return function() { clearTimeout(id); };
		});
	}
	
	
	// STEP PROCESSES
	
	function step(numSteps, process)
	{
		while (numSteps < MAX_STEPS)
		{
			var ctor = process.root.ctor;
	
			if (ctor === '_Task_succeed')
			{
				while (process.stack && process.stack.ctor === '_Task_onError')
				{
					process.stack = process.stack.rest;
				}
				if (process.stack === null)
				{
					break;
				}
				process.root = process.stack.callback(process.root.value);
				process.stack = process.stack.rest;
				++numSteps;
				continue;
			}
	
			if (ctor === '_Task_fail')
			{
				while (process.stack && process.stack.ctor === '_Task_andThen')
				{
					process.stack = process.stack.rest;
				}
				if (process.stack === null)
				{
					break;
				}
				process.root = process.stack.callback(process.root.value);
				process.stack = process.stack.rest;
				++numSteps;
				continue;
			}
	
			if (ctor === '_Task_andThen')
			{
				process.stack = {
					ctor: '_Task_andThen',
					callback: process.root.callback,
					rest: process.stack
				};
				process.root = process.root.task;
				++numSteps;
				continue;
			}
	
			if (ctor === '_Task_onError')
			{
				process.stack = {
					ctor: '_Task_onError',
					callback: process.root.callback,
					rest: process.stack
				};
				process.root = process.root.task;
				++numSteps;
				continue;
			}
	
			if (ctor === '_Task_nativeBinding')
			{
				process.root.cancel = process.root.callback(function(newRoot) {
					process.root = newRoot;
					enqueue(process);
				});
	
				break;
			}
	
			if (ctor === '_Task_receive')
			{
				var mailbox = process.mailbox;
				if (mailbox.length === 0)
				{
					break;
				}
	
				process.root = process.root.callback(mailbox.shift());
				++numSteps;
				continue;
			}
	
			throw new Error(ctor);
		}
	
		if (numSteps < MAX_STEPS)
		{
			return numSteps + 1;
		}
		enqueue(process);
	
		return numSteps;
	}
	
	
	// WORK QUEUE
	
	var working = false;
	var workQueue = [];
	
	function enqueue(process)
	{
		workQueue.push(process);
	
		if (!working)
		{
			setTimeout(work, 0);
			working = true;
		}
	}
	
	function work()
	{
		var numSteps = 0;
		var process;
		while (numSteps < MAX_STEPS && (process = workQueue.shift()))
		{
			numSteps = step(numSteps, process);
		}
		if (!process)
		{
			working = false;
			return;
		}
		setTimeout(work, 0);
	}
	
	
	return {
		succeed: succeed,
		fail: fail,
		nativeBinding: nativeBinding,
		andThen: F2(andThen),
		onError: F2(onError),
		receive: receive,
	
		spawn: spawn,
		kill: kill,
		sleep: sleep,
		send: F2(send),
	
		rawSpawn: rawSpawn,
		rawSend: rawSend
	};
	
	}();
	var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
	var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
	var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
	var _elm_lang$core$Platform$Program = {ctor: 'Program'};
	var _elm_lang$core$Platform$Task = {ctor: 'Task'};
	var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
	var _elm_lang$core$Platform$Router = {ctor: 'Router'};
	
	var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
	var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
		_elm_lang$core$Native_List.fromArray(
			[]));
	var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
	_elm_lang$core$Platform_Cmd_ops['!'] = F2(
		function (model, commands) {
			return {
				ctor: '_Tuple2',
				_0: model,
				_1: _elm_lang$core$Platform_Cmd$batch(commands)
			};
		});
	var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
	var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};
	
	var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
	var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
		_elm_lang$core$Native_List.fromArray(
			[]));
	var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
	var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};
	
	//import Native.List //
	
	var _elm_lang$core$Native_Array = function() {
	
	// A RRB-Tree has two distinct data types.
	// Leaf -> "height"  is always 0
	//         "table"   is an array of elements
	// Node -> "height"  is always greater than 0
	//         "table"   is an array of child nodes
	//         "lengths" is an array of accumulated lengths of the child nodes
	
	// M is the maximal table size. 32 seems fast. E is the allowed increase
	// of search steps when concatting to find an index. Lower values will
	// decrease balancing, but will increase search steps.
	var M = 32;
	var E = 2;
	
	// An empty array.
	var empty = {
		ctor: '_Array',
		height: 0,
		table: []
	};
	
	
	function get(i, array)
	{
		if (i < 0 || i >= length(array))
		{
			throw new Error(
				'Index ' + i + ' is out of range. Check the length of ' +
				'your array first or use getMaybe or getWithDefault.');
		}
		return unsafeGet(i, array);
	}
	
	
	function unsafeGet(i, array)
	{
		for (var x = array.height; x > 0; x--)
		{
			var slot = i >> (x * 5);
			while (array.lengths[slot] <= i)
			{
				slot++;
			}
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array = array.table[slot];
		}
		return array.table[i];
	}
	
	
	// Sets the value at the index i. Only the nodes leading to i will get
	// copied and updated.
	function set(i, item, array)
	{
		if (i < 0 || length(array) <= i)
		{
			return array;
		}
		return unsafeSet(i, item, array);
	}
	
	
	function unsafeSet(i, item, array)
	{
		array = nodeCopy(array);
	
		if (array.height === 0)
		{
			array.table[i] = item;
		}
		else
		{
			var slot = getSlot(i, array);
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array.table[slot] = unsafeSet(i, item, array.table[slot]);
		}
		return array;
	}
	
	
	function initialize(len, f)
	{
		if (len <= 0)
		{
			return empty;
		}
		var h = Math.floor( Math.log(len) / Math.log(M) );
		return initialize_(f, h, 0, len);
	}
	
	function initialize_(f, h, from, to)
	{
		if (h === 0)
		{
			var table = new Array((to - from) % (M + 1));
			for (var i = 0; i < table.length; i++)
			{
			  table[i] = f(from + i);
			}
			return {
				ctor: '_Array',
				height: 0,
				table: table
			};
		}
	
		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
		}
		return {
			ctor: '_Array',
			height: h,
			table: table,
			lengths: lengths
		};
	}
	
	function fromList(list)
	{
		if (list.ctor === '[]')
		{
			return empty;
		}
	
		// Allocate M sized blocks (table) and write list elements to it.
		var table = new Array(M);
		var nodes = [];
		var i = 0;
	
		while (list.ctor !== '[]')
		{
			table[i] = list._0;
			list = list._1;
			i++;
	
			// table is full, so we can push a leaf containing it into the
			// next node.
			if (i === M)
			{
				var leaf = {
					ctor: '_Array',
					height: 0,
					table: table
				};
				fromListPush(leaf, nodes);
				table = new Array(M);
				i = 0;
			}
		}
	
		// Maybe there is something left on the table.
		if (i > 0)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table.splice(0, i)
			};
			fromListPush(leaf, nodes);
		}
	
		// Go through all of the nodes and eventually push them into higher nodes.
		for (var h = 0; h < nodes.length - 1; h++)
		{
			if (nodes[h].table.length > 0)
			{
				fromListPush(nodes[h], nodes);
			}
		}
	
		var head = nodes[nodes.length - 1];
		if (head.height > 0 && head.table.length === 1)
		{
			return head.table[0];
		}
		else
		{
			return head;
		}
	}
	
	// Push a node into a higher node as a child.
	function fromListPush(toPush, nodes)
	{
		var h = toPush.height;
	
		// Maybe the node on this height does not exist.
		if (nodes.length === h)
		{
			var node = {
				ctor: '_Array',
				height: h + 1,
				table: [],
				lengths: []
			};
			nodes.push(node);
		}
	
		nodes[h].table.push(toPush);
		var len = length(toPush);
		if (nodes[h].lengths.length > 0)
		{
			len += nodes[h].lengths[nodes[h].lengths.length - 1];
		}
		nodes[h].lengths.push(len);
	
		if (nodes[h].table.length === M)
		{
			fromListPush(nodes[h], nodes);
			nodes[h] = {
				ctor: '_Array',
				height: h + 1,
				table: [],
				lengths: []
			};
		}
	}
	
	// Pushes an item via push_ to the bottom right of a tree.
	function push(item, a)
	{
		var pushed = push_(item, a);
		if (pushed !== null)
		{
			return pushed;
		}
	
		var newTree = create(item, a.height);
		return siblise(a, newTree);
	}
	
	// Recursively tries to push an item to the bottom-right most
	// tree possible. If there is no space left for the item,
	// null will be returned.
	function push_(item, a)
	{
		// Handle resursion stop at leaf level.
		if (a.height === 0)
		{
			if (a.table.length < M)
			{
				var newA = {
					ctor: '_Array',
					height: 0,
					table: a.table.slice()
				};
				newA.table.push(item);
				return newA;
			}
			else
			{
			  return null;
			}
		}
	
		// Recursively push
		var pushed = push_(item, botRight(a));
	
		// There was space in the bottom right tree, so the slot will
		// be updated.
		if (pushed !== null)
		{
			var newA = nodeCopy(a);
			newA.table[newA.table.length - 1] = pushed;
			newA.lengths[newA.lengths.length - 1]++;
			return newA;
		}
	
		// When there was no space left, check if there is space left
		// for a new slot with a tree which contains only the item
		// at the bottom.
		if (a.table.length < M)
		{
			var newSlot = create(item, a.height - 1);
			var newA = nodeCopy(a);
			newA.table.push(newSlot);
			newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
			return newA;
		}
		else
		{
			return null;
		}
	}
	
	// Converts an array into a list of elements.
	function toList(a)
	{
		return toList_(_elm_lang$core$Native_List.Nil, a);
	}
	
	function toList_(list, a)
	{
		for (var i = a.table.length - 1; i >= 0; i--)
		{
			list =
				a.height === 0
					? _elm_lang$core$Native_List.Cons(a.table[i], list)
					: toList_(list, a.table[i]);
		}
		return list;
	}
	
	// Maps a function over the elements of an array.
	function map(f, a)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height === 0
					? f(a.table[i])
					: map(f, a.table[i]);
		}
		return newA;
	}
	
	// Maps a function over the elements with their index as first argument.
	function indexedMap(f, a)
	{
		return indexedMap_(f, a, 0);
	}
	
	function indexedMap_(f, a, from)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height === 0
					? A2(f, from + i, a.table[i])
					: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
		}
		return newA;
	}
	
	function foldl(f, b, a)
	{
		if (a.height === 0)
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = foldl(f, b, a.table[i]);
			}
		}
		return b;
	}
	
	function foldr(f, b, a)
	{
		if (a.height === 0)
		{
			for (var i = a.table.length; i--; )
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = a.table.length; i--; )
			{
				b = foldr(f, b, a.table[i]);
			}
		}
		return b;
	}
	
	// TODO: currently, it slices the right, then the left. This can be
	// optimized.
	function slice(from, to, a)
	{
		if (from < 0)
		{
			from += length(a);
		}
		if (to < 0)
		{
			to += length(a);
		}
		return sliceLeft(from, sliceRight(to, a));
	}
	
	function sliceRight(to, a)
	{
		if (to === length(a))
		{
			return a;
		}
	
		// Handle leaf level.
		if (a.height === 0)
		{
			var newA = { ctor:'_Array', height:0 };
			newA.table = a.table.slice(0, to);
			return newA;
		}
	
		// Slice the right recursively.
		var right = getSlot(to, a);
		var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);
	
		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (right === 0)
		{
			return sliced;
		}
	
		// Create new node.
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice(0, right),
			lengths: a.lengths.slice(0, right)
		};
		if (sliced.table.length > 0)
		{
			newA.table[right] = sliced;
			newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
		}
		return newA;
	}
	
	function sliceLeft(from, a)
	{
		if (from === 0)
		{
			return a;
		}
	
		// Handle leaf level.
		if (a.height === 0)
		{
			var newA = { ctor:'_Array', height:0 };
			newA.table = a.table.slice(from, a.table.length + 1);
			return newA;
		}
	
		// Slice the left recursively.
		var left = getSlot(from, a);
		var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);
	
		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (left === a.table.length - 1)
		{
			return sliced;
		}
	
		// Create new node.
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice(left, a.table.length + 1),
			lengths: new Array(a.table.length - left)
		};
		newA.table[0] = sliced;
		var len = 0;
		for (var i = 0; i < newA.table.length; i++)
		{
			len += length(newA.table[i]);
			newA.lengths[i] = len;
		}
	
		return newA;
	}
	
	// Appends two trees.
	function append(a,b)
	{
		if (a.table.length === 0)
		{
			return b;
		}
		if (b.table.length === 0)
		{
			return a;
		}
	
		var c = append_(a, b);
	
		// Check if both nodes can be crunshed together.
		if (c[0].table.length + c[1].table.length <= M)
		{
			if (c[0].table.length === 0)
			{
				return c[1];
			}
			if (c[1].table.length === 0)
			{
				return c[0];
			}
	
			// Adjust .table and .lengths
			c[0].table = c[0].table.concat(c[1].table);
			if (c[0].height > 0)
			{
				var len = length(c[0]);
				for (var i = 0; i < c[1].lengths.length; i++)
				{
					c[1].lengths[i] += len;
				}
				c[0].lengths = c[0].lengths.concat(c[1].lengths);
			}
	
			return c[0];
		}
	
		if (c[0].height > 0)
		{
			var toRemove = calcToRemove(a, b);
			if (toRemove > E)
			{
				c = shuffle(c[0], c[1], toRemove);
			}
		}
	
		return siblise(c[0], c[1]);
	}
	
	// Returns an array of two nodes; right and left. One node _may_ be empty.
	function append_(a, b)
	{
		if (a.height === 0 && b.height === 0)
		{
			return [a, b];
		}
	
		if (a.height !== 1 || b.height !== 1)
		{
			if (a.height === b.height)
			{
				a = nodeCopy(a);
				b = nodeCopy(b);
				var appended = append_(botRight(a), botLeft(b));
	
				insertRight(a, appended[1]);
				insertLeft(b, appended[0]);
			}
			else if (a.height > b.height)
			{
				a = nodeCopy(a);
				var appended = append_(botRight(a), b);
	
				insertRight(a, appended[0]);
				b = parentise(appended[1], appended[1].height + 1);
			}
			else
			{
				b = nodeCopy(b);
				var appended = append_(a, botLeft(b));
	
				var left = appended[0].table.length === 0 ? 0 : 1;
				var right = left === 0 ? 1 : 0;
				insertLeft(b, appended[left]);
				a = parentise(appended[right], appended[right].height + 1);
			}
		}
	
		// Check if balancing is needed and return based on that.
		if (a.table.length === 0 || b.table.length === 0)
		{
			return [a, b];
		}
	
		var toRemove = calcToRemove(a, b);
		if (toRemove <= E)
		{
			return [a, b];
		}
		return shuffle(a, b, toRemove);
	}
	
	// Helperfunctions for append_. Replaces a child node at the side of the parent.
	function insertRight(parent, node)
	{
		var index = parent.table.length - 1;
		parent.table[index] = node;
		parent.lengths[index] = length(node);
		parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
	}
	
	function insertLeft(parent, node)
	{
		if (node.table.length > 0)
		{
			parent.table[0] = node;
			parent.lengths[0] = length(node);
	
			var len = length(parent.table[0]);
			for (var i = 1; i < parent.lengths.length; i++)
			{
				len += length(parent.table[i]);
				parent.lengths[i] = len;
			}
		}
		else
		{
			parent.table.shift();
			for (var i = 1; i < parent.lengths.length; i++)
			{
				parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
			}
			parent.lengths.shift();
		}
	}
	
	// Returns the extra search steps for E. Refer to the paper.
	function calcToRemove(a, b)
	{
		var subLengths = 0;
		for (var i = 0; i < a.table.length; i++)
		{
			subLengths += a.table[i].table.length;
		}
		for (var i = 0; i < b.table.length; i++)
		{
			subLengths += b.table[i].table.length;
		}
	
		var toRemove = a.table.length + b.table.length;
		return toRemove - (Math.floor((subLengths - 1) / M) + 1);
	}
	
	// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
	function get2(a, b, index)
	{
		return index < a.length
			? a[index]
			: b[index - a.length];
	}
	
	function set2(a, b, index, value)
	{
		if (index < a.length)
		{
			a[index] = value;
		}
		else
		{
			b[index - a.length] = value;
		}
	}
	
	function saveSlot(a, b, index, slot)
	{
		set2(a.table, b.table, index, slot);
	
		var l = (index === 0 || index === a.lengths.length)
			? 0
			: get2(a.lengths, a.lengths, index - 1);
	
		set2(a.lengths, b.lengths, index, l + length(slot));
	}
	
	// Creates a node or leaf with a given length at their arrays for perfomance.
	// Is only used by shuffle.
	function createNode(h, length)
	{
		if (length < 0)
		{
			length = 0;
		}
		var a = {
			ctor: '_Array',
			height: h,
			table: new Array(length)
		};
		if (h > 0)
		{
			a.lengths = new Array(length);
		}
		return a;
	}
	
	// Returns an array of two balanced nodes.
	function shuffle(a, b, toRemove)
	{
		var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
		var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));
	
		// Skip the slots with size M. More precise: copy the slot references
		// to the new node
		var read = 0;
		while (get2(a.table, b.table, read).table.length % M === 0)
		{
			set2(newA.table, newB.table, read, get2(a.table, b.table, read));
			set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
			read++;
		}
	
		// Pulling items from left to right, caching in a slot before writing
		// it into the new nodes.
		var write = read;
		var slot = new createNode(a.height - 1, 0);
		var from = 0;
	
		// If the current slot is still containing data, then there will be at
		// least one more write, so we do not break this loop yet.
		while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
		{
			// Find out the max possible items for copying.
			var source = get2(a.table, b.table, read);
			var to = Math.min(M - slot.table.length, source.table.length);
	
			// Copy and adjust size table.
			slot.table = slot.table.concat(source.table.slice(from, to));
			if (slot.height > 0)
			{
				var len = slot.lengths.length;
				for (var i = len; i < len + to - from; i++)
				{
					slot.lengths[i] = length(slot.table[i]);
					slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
				}
			}
	
			from += to;
	
			// Only proceed to next slots[i] if the current one was
			// fully copied.
			if (source.table.length <= to)
			{
				read++; from = 0;
			}
	
			// Only create a new slot if the current one is filled up.
			if (slot.table.length === M)
			{
				saveSlot(newA, newB, write, slot);
				slot = createNode(a.height - 1, 0);
				write++;
			}
		}
	
		// Cleanup after the loop. Copy the last slot into the new nodes.
		if (slot.table.length > 0)
		{
			saveSlot(newA, newB, write, slot);
			write++;
		}
	
		// Shift the untouched slots to the left
		while (read < a.table.length + b.table.length )
		{
			saveSlot(newA, newB, write, get2(a.table, b.table, read));
			read++;
			write++;
		}
	
		return [newA, newB];
	}
	
	// Navigation functions
	function botRight(a)
	{
		return a.table[a.table.length - 1];
	}
	function botLeft(a)
	{
		return a.table[0];
	}
	
	// Copies a node for updating. Note that you should not use this if
	// only updating only one of "table" or "lengths" for performance reasons.
	function nodeCopy(a)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice()
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths.slice();
		}
		return newA;
	}
	
	// Returns how many items are in the tree.
	function length(array)
	{
		if (array.height === 0)
		{
			return array.table.length;
		}
		else
		{
			return array.lengths[array.lengths.length - 1];
		}
	}
	
	// Calculates in which slot of "table" the item probably is, then
	// find the exact slot via forward searching in  "lengths". Returns the index.
	function getSlot(i, a)
	{
		var slot = i >> (5 * a.height);
		while (a.lengths[slot] <= i)
		{
			slot++;
		}
		return slot;
	}
	
	// Recursively creates a tree with a given height containing
	// only the given item.
	function create(item, h)
	{
		if (h === 0)
		{
			return {
				ctor: '_Array',
				height: 0,
				table: [item]
			};
		}
		return {
			ctor: '_Array',
			height: h,
			table: [create(item, h - 1)],
			lengths: [1]
		};
	}
	
	// Recursively creates a tree that contains the given tree.
	function parentise(tree, h)
	{
		if (h === tree.height)
		{
			return tree;
		}
	
		return {
			ctor: '_Array',
			height: h,
			table: [parentise(tree, h - 1)],
			lengths: [length(tree)]
		};
	}
	
	// Emphasizes blood brotherhood beneath two trees.
	function siblise(a, b)
	{
		return {
			ctor: '_Array',
			height: a.height + 1,
			table: [a, b],
			lengths: [length(a), length(a) + length(b)]
		};
	}
	
	function toJSArray(a)
	{
		var jsArray = new Array(length(a));
		toJSArray_(jsArray, 0, a);
		return jsArray;
	}
	
	function toJSArray_(jsArray, i, a)
	{
		for (var t = 0; t < a.table.length; t++)
		{
			if (a.height === 0)
			{
				jsArray[i + t] = a.table[t];
			}
			else
			{
				var inc = t === 0 ? 0 : a.lengths[t - 1];
				toJSArray_(jsArray, i + inc, a.table[t]);
			}
		}
	}
	
	function fromJSArray(jsArray)
	{
		if (jsArray.length === 0)
		{
			return empty;
		}
		var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
		return fromJSArray_(jsArray, h, 0, jsArray.length);
	}
	
	function fromJSArray_(jsArray, h, from, to)
	{
		if (h === 0)
		{
			return {
				ctor: '_Array',
				height: 0,
				table: jsArray.slice(from, to)
			};
		}
	
		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
		}
		return {
			ctor: '_Array',
			height: h,
			table: table,
			lengths: lengths
		};
	}
	
	return {
		empty: empty,
		fromList: fromList,
		toList: toList,
		initialize: F2(initialize),
		append: F2(append),
		push: F2(push),
		slice: F3(slice),
		get: F2(get),
		set: F3(set),
		map: F2(map),
		indexedMap: F2(indexedMap),
		foldl: F3(foldl),
		foldr: F3(foldr),
		length: length,
	
		toJSArray: toJSArray,
		fromJSArray: fromJSArray
	};
	
	}();
	var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
	var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
	var _elm_lang$core$Array$isEmpty = function (array) {
		return _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$Array$length(array),
			0);
	};
	var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
	var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
	var _elm_lang$core$Array$get = F2(
		function (i, array) {
			return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
				i,
				_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
				A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
		});
	var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
	var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
	var _elm_lang$core$Array$filter = F2(
		function (isOkay, arr) {
			var update = F2(
				function (x, xs) {
					return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
				});
			return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
		});
	var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
	var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
	var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
	var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
	var _elm_lang$core$Array$toIndexedList = function (array) {
		return A3(
			_elm_lang$core$List$map2,
			F2(
				function (v0, v1) {
					return {ctor: '_Tuple2', _0: v0, _1: v1};
				}),
			_elm_lang$core$Native_List.range(
				0,
				_elm_lang$core$Native_Array.length(array) - 1),
			_elm_lang$core$Native_Array.toList(array));
	};
	var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
	var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
	var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
	var _elm_lang$core$Array$repeat = F2(
		function (n, e) {
			return A2(
				_elm_lang$core$Array$initialize,
				n,
				_elm_lang$core$Basics$always(e));
		});
	var _elm_lang$core$Array$Array = {ctor: 'Array'};
	
	var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
	var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
	var _elm_lang$core$Task$spawnCmd = F2(
		function (router, _p0) {
			var _p1 = _p0;
			return _elm_lang$core$Native_Scheduler.spawn(
				A2(
					_elm_lang$core$Task$andThen,
					_p1._0,
					_elm_lang$core$Platform$sendToApp(router)));
		});
	var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
	var _elm_lang$core$Task$mapError = F2(
		function (f, task) {
			return A2(
				_elm_lang$core$Task$onError,
				task,
				function (err) {
					return _elm_lang$core$Task$fail(
						f(err));
				});
		});
	var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
	var _elm_lang$core$Task$map = F2(
		function (func, taskA) {
			return A2(
				_elm_lang$core$Task$andThen,
				taskA,
				function (a) {
					return _elm_lang$core$Task$succeed(
						func(a));
				});
		});
	var _elm_lang$core$Task$map2 = F3(
		function (func, taskA, taskB) {
			return A2(
				_elm_lang$core$Task$andThen,
				taskA,
				function (a) {
					return A2(
						_elm_lang$core$Task$andThen,
						taskB,
						function (b) {
							return _elm_lang$core$Task$succeed(
								A2(func, a, b));
						});
				});
		});
	var _elm_lang$core$Task$map3 = F4(
		function (func, taskA, taskB, taskC) {
			return A2(
				_elm_lang$core$Task$andThen,
				taskA,
				function (a) {
					return A2(
						_elm_lang$core$Task$andThen,
						taskB,
						function (b) {
							return A2(
								_elm_lang$core$Task$andThen,
								taskC,
								function (c) {
									return _elm_lang$core$Task$succeed(
										A3(func, a, b, c));
								});
						});
				});
		});
	var _elm_lang$core$Task$map4 = F5(
		function (func, taskA, taskB, taskC, taskD) {
			return A2(
				_elm_lang$core$Task$andThen,
				taskA,
				function (a) {
					return A2(
						_elm_lang$core$Task$andThen,
						taskB,
						function (b) {
							return A2(
								_elm_lang$core$Task$andThen,
								taskC,
								function (c) {
									return A2(
										_elm_lang$core$Task$andThen,
										taskD,
										function (d) {
											return _elm_lang$core$Task$succeed(
												A4(func, a, b, c, d));
										});
								});
						});
				});
		});
	var _elm_lang$core$Task$map5 = F6(
		function (func, taskA, taskB, taskC, taskD, taskE) {
			return A2(
				_elm_lang$core$Task$andThen,
				taskA,
				function (a) {
					return A2(
						_elm_lang$core$Task$andThen,
						taskB,
						function (b) {
							return A2(
								_elm_lang$core$Task$andThen,
								taskC,
								function (c) {
									return A2(
										_elm_lang$core$Task$andThen,
										taskD,
										function (d) {
											return A2(
												_elm_lang$core$Task$andThen,
												taskE,
												function (e) {
													return _elm_lang$core$Task$succeed(
														A5(func, a, b, c, d, e));
												});
										});
								});
						});
				});
		});
	var _elm_lang$core$Task$andMap = F2(
		function (taskFunc, taskValue) {
			return A2(
				_elm_lang$core$Task$andThen,
				taskFunc,
				function (func) {
					return A2(
						_elm_lang$core$Task$andThen,
						taskValue,
						function (value) {
							return _elm_lang$core$Task$succeed(
								func(value));
						});
				});
		});
	var _elm_lang$core$Task$sequence = function (tasks) {
		var _p2 = tasks;
		if (_p2.ctor === '[]') {
			return _elm_lang$core$Task$succeed(
				_elm_lang$core$Native_List.fromArray(
					[]));
		} else {
			return A3(
				_elm_lang$core$Task$map2,
				F2(
					function (x, y) {
						return A2(_elm_lang$core$List_ops['::'], x, y);
					}),
				_p2._0,
				_elm_lang$core$Task$sequence(_p2._1));
		}
	};
	var _elm_lang$core$Task$onEffects = F3(
		function (router, commands, state) {
			return A2(
				_elm_lang$core$Task$map,
				function (_p3) {
					return {ctor: '_Tuple0'};
				},
				_elm_lang$core$Task$sequence(
					A2(
						_elm_lang$core$List$map,
						_elm_lang$core$Task$spawnCmd(router),
						commands)));
		});
	var _elm_lang$core$Task$toMaybe = function (task) {
		return A2(
			_elm_lang$core$Task$onError,
			A2(_elm_lang$core$Task$map, _elm_lang$core$Maybe$Just, task),
			function (_p4) {
				return _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
			});
	};
	var _elm_lang$core$Task$fromMaybe = F2(
		function ($default, maybe) {
			var _p5 = maybe;
			if (_p5.ctor === 'Just') {
				return _elm_lang$core$Task$succeed(_p5._0);
			} else {
				return _elm_lang$core$Task$fail($default);
			}
		});
	var _elm_lang$core$Task$toResult = function (task) {
		return A2(
			_elm_lang$core$Task$onError,
			A2(_elm_lang$core$Task$map, _elm_lang$core$Result$Ok, task),
			function (msg) {
				return _elm_lang$core$Task$succeed(
					_elm_lang$core$Result$Err(msg));
			});
	};
	var _elm_lang$core$Task$fromResult = function (result) {
		var _p6 = result;
		if (_p6.ctor === 'Ok') {
			return _elm_lang$core$Task$succeed(_p6._0);
		} else {
			return _elm_lang$core$Task$fail(_p6._0);
		}
	};
	var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
		{ctor: '_Tuple0'});
	var _elm_lang$core$Task$onSelfMsg = F3(
		function (_p9, _p8, _p7) {
			return _elm_lang$core$Task$succeed(
				{ctor: '_Tuple0'});
		});
	var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
	var _elm_lang$core$Task$T = function (a) {
		return {ctor: 'T', _0: a};
	};
	var _elm_lang$core$Task$perform = F3(
		function (onFail, onSuccess, task) {
			return _elm_lang$core$Task$command(
				_elm_lang$core$Task$T(
					A2(
						_elm_lang$core$Task$onError,
						A2(_elm_lang$core$Task$map, onSuccess, task),
						function (x) {
							return _elm_lang$core$Task$succeed(
								onFail(x));
						})));
		});
	var _elm_lang$core$Task$cmdMap = F2(
		function (tagger, _p10) {
			var _p11 = _p10;
			return _elm_lang$core$Task$T(
				A2(_elm_lang$core$Task$map, tagger, _p11._0));
		});
	_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};
	
	//import Maybe, Native.List, Native.Utils, Result //
	
	var _elm_lang$core$Native_String = function() {
	
	function isEmpty(str)
	{
		return str.length === 0;
	}
	function cons(chr, str)
	{
		return chr + str;
	}
	function uncons(str)
	{
		var hd = str[0];
		if (hd)
		{
			return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
		}
		return _elm_lang$core$Maybe$Nothing;
	}
	function append(a, b)
	{
		return a + b;
	}
	function concat(strs)
	{
		return _elm_lang$core$Native_List.toArray(strs).join('');
	}
	function length(str)
	{
		return str.length;
	}
	function map(f, str)
	{
		var out = str.split('');
		for (var i = out.length; i--; )
		{
			out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
		}
		return out.join('');
	}
	function filter(pred, str)
	{
		return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
	}
	function reverse(str)
	{
		return str.split('').reverse().join('');
	}
	function foldl(f, b, str)
	{
		var len = str.length;
		for (var i = 0; i < len; ++i)
		{
			b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
		}
		return b;
	}
	function foldr(f, b, str)
	{
		for (var i = str.length; i--; )
		{
			b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
		}
		return b;
	}
	function split(sep, str)
	{
		return _elm_lang$core$Native_List.fromArray(str.split(sep));
	}
	function join(sep, strs)
	{
		return _elm_lang$core$Native_List.toArray(strs).join(sep);
	}
	function repeat(n, str)
	{
		var result = '';
		while (n > 0)
		{
			if (n & 1)
			{
				result += str;
			}
			n >>= 1, str += str;
		}
		return result;
	}
	function slice(start, end, str)
	{
		return str.slice(start, end);
	}
	function left(n, str)
	{
		return n < 1 ? '' : str.slice(0, n);
	}
	function right(n, str)
	{
		return n < 1 ? '' : str.slice(-n);
	}
	function dropLeft(n, str)
	{
		return n < 1 ? str : str.slice(n);
	}
	function dropRight(n, str)
	{
		return n < 1 ? str : str.slice(0, -n);
	}
	function pad(n, chr, str)
	{
		var half = (n - str.length) / 2;
		return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
	}
	function padRight(n, chr, str)
	{
		return str + repeat(n - str.length, chr);
	}
	function padLeft(n, chr, str)
	{
		return repeat(n - str.length, chr) + str;
	}
	
	function trim(str)
	{
		return str.trim();
	}
	function trimLeft(str)
	{
		return str.replace(/^\s+/, '');
	}
	function trimRight(str)
	{
		return str.replace(/\s+$/, '');
	}
	
	function words(str)
	{
		return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
	}
	function lines(str)
	{
		return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
	}
	
	function toUpper(str)
	{
		return str.toUpperCase();
	}
	function toLower(str)
	{
		return str.toLowerCase();
	}
	
	function any(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
			{
				return true;
			}
		}
		return false;
	}
	function all(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
			{
				return false;
			}
		}
		return true;
	}
	
	function contains(sub, str)
	{
		return str.indexOf(sub) > -1;
	}
	function startsWith(sub, str)
	{
		return str.indexOf(sub) === 0;
	}
	function endsWith(sub, str)
	{
		return str.length >= sub.length &&
			str.lastIndexOf(sub) === str.length - sub.length;
	}
	function indexes(sub, str)
	{
		var subLen = sub.length;
		var i = 0;
		var is = [];
		while ((i = str.indexOf(sub, i)) > -1)
		{
			is.push(i);
			i = i + subLen;
		}
		return _elm_lang$core$Native_List.fromArray(is);
	}
	
	function toInt(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int" );
		}
		var start = 0;
		if (s[0] === '-')
		{
			if (len === 1)
			{
				return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int" );
			}
			start = 1;
		}
		for (var i = start; i < len; ++i)
		{
			var c = s[i];
			if (c < '0' || '9' < c)
			{
				return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int" );
			}
		}
		return _elm_lang$core$Result$Ok(parseInt(s, 10));
	}
	
	function toFloat(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float" );
		}
		var start = 0;
		if (s[0] === '-')
		{
			if (len === 1)
			{
				return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float" );
			}
			start = 1;
		}
		var dotCount = 0;
		for (var i = start; i < len; ++i)
		{
			var c = s[i];
			if ('0' <= c && c <= '9')
			{
				continue;
			}
			if (c === '.')
			{
				dotCount += 1;
				if (dotCount <= 1)
				{
					continue;
				}
			}
			return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float" );
		}
		return _elm_lang$core$Result$Ok(parseFloat(s));
	}
	
	function toList(str)
	{
		return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
	}
	function fromList(chars)
	{
		return _elm_lang$core$Native_List.toArray(chars).join('');
	}
	
	return {
		isEmpty: isEmpty,
		cons: F2(cons),
		uncons: uncons,
		append: F2(append),
		concat: concat,
		length: length,
		map: F2(map),
		filter: F2(filter),
		reverse: reverse,
		foldl: F3(foldl),
		foldr: F3(foldr),
	
		split: F2(split),
		join: F2(join),
		repeat: F2(repeat),
	
		slice: F3(slice),
		left: F2(left),
		right: F2(right),
		dropLeft: F2(dropLeft),
		dropRight: F2(dropRight),
	
		pad: F3(pad),
		padLeft: F3(padLeft),
		padRight: F3(padRight),
	
		trim: trim,
		trimLeft: trimLeft,
		trimRight: trimRight,
	
		words: words,
		lines: lines,
	
		toUpper: toUpper,
		toLower: toLower,
	
		any: F2(any),
		all: F2(all),
	
		contains: F2(contains),
		startsWith: F2(startsWith),
		endsWith: F2(endsWith),
		indexes: F2(indexes),
	
		toInt: toInt,
		toFloat: toFloat,
		toList: toList,
		fromList: fromList
	};
	
	}();
	//import Native.Utils //
	
	var _elm_lang$core$Native_Char = function() {
	
	return {
		fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
		toCode: function(c) { return c.charCodeAt(0); },
		toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
		toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
		toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
		toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
	};
	
	}();
	var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
	var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
	var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
	var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
	var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
	var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
	var _elm_lang$core$Char$isBetween = F3(
		function (low, high, $char) {
			var code = _elm_lang$core$Char$toCode($char);
			return (_elm_lang$core$Native_Utils.cmp(
				code,
				_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
				code,
				_elm_lang$core$Char$toCode(high)) < 1);
		});
	var _elm_lang$core$Char$isUpper = A2(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('Z'));
	var _elm_lang$core$Char$isLower = A2(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('z'));
	var _elm_lang$core$Char$isDigit = A2(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('0'),
		_elm_lang$core$Native_Utils.chr('9'));
	var _elm_lang$core$Char$isOctDigit = A2(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('0'),
		_elm_lang$core$Native_Utils.chr('7'));
	var _elm_lang$core$Char$isHexDigit = function ($char) {
		return _elm_lang$core$Char$isDigit($char) || (A3(
			_elm_lang$core$Char$isBetween,
			_elm_lang$core$Native_Utils.chr('a'),
			_elm_lang$core$Native_Utils.chr('f'),
			$char) || A3(
			_elm_lang$core$Char$isBetween,
			_elm_lang$core$Native_Utils.chr('A'),
			_elm_lang$core$Native_Utils.chr('F'),
			$char));
	};
	
	var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
	var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
	var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
	var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
	var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
	var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
	var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
	var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
	var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
	var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
	var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
	var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
	var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
	var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
	var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
	var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
	var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
	var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
	var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
	var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
	var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
	var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
	var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
	var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
	var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
	var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
	var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
	var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
	var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
	var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
	var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
	var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
	var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
	var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
	var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
	var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
	var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
	var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
	var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
	var _elm_lang$core$String$fromChar = function ($char) {
		return A2(_elm_lang$core$String$cons, $char, '');
	};
	var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;
	
	var _elm_lang$core$Dict$foldr = F3(
		function (f, acc, t) {
			foldr:
			while (true) {
				var _p0 = t;
				if (_p0.ctor === 'RBEmpty_elm_builtin') {
					return acc;
				} else {
					var _v1 = f,
						_v2 = A3(
						f,
						_p0._1,
						_p0._2,
						A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
						_v3 = _p0._3;
					f = _v1;
					acc = _v2;
					t = _v3;
					continue foldr;
				}
			}
		});
	var _elm_lang$core$Dict$keys = function (dict) {
		return A3(
			_elm_lang$core$Dict$foldr,
			F3(
				function (key, value, keyList) {
					return A2(_elm_lang$core$List_ops['::'], key, keyList);
				}),
			_elm_lang$core$Native_List.fromArray(
				[]),
			dict);
	};
	var _elm_lang$core$Dict$values = function (dict) {
		return A3(
			_elm_lang$core$Dict$foldr,
			F3(
				function (key, value, valueList) {
					return A2(_elm_lang$core$List_ops['::'], value, valueList);
				}),
			_elm_lang$core$Native_List.fromArray(
				[]),
			dict);
	};
	var _elm_lang$core$Dict$toList = function (dict) {
		return A3(
			_elm_lang$core$Dict$foldr,
			F3(
				function (key, value, list) {
					return A2(
						_elm_lang$core$List_ops['::'],
						{ctor: '_Tuple2', _0: key, _1: value},
						list);
				}),
			_elm_lang$core$Native_List.fromArray(
				[]),
			dict);
	};
	var _elm_lang$core$Dict$foldl = F3(
		function (f, acc, dict) {
			foldl:
			while (true) {
				var _p1 = dict;
				if (_p1.ctor === 'RBEmpty_elm_builtin') {
					return acc;
				} else {
					var _v5 = f,
						_v6 = A3(
						f,
						_p1._1,
						_p1._2,
						A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
						_v7 = _p1._4;
					f = _v5;
					acc = _v6;
					dict = _v7;
					continue foldl;
				}
			}
		});
	var _elm_lang$core$Dict$merge = F6(
		function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
			var stepState = F3(
				function (rKey, rValue, _p2) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						return (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) ? {
							ctor: '_Tuple2',
							_0: _p7,
							_1: A3(leftStep, _p5, _p6, _p9)
						} : ((_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) ? {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						} : {
							ctor: '_Tuple2',
							_0: _p7,
							_1: A4(bothStep, _p5, _p6, rValue, _p9)
						});
					}
				});
			var _p10 = A3(
				_elm_lang$core$Dict$foldl,
				stepState,
				{
					ctor: '_Tuple2',
					_0: _elm_lang$core$Dict$toList(leftDict),
					_1: initialResult
				},
				rightDict);
			var leftovers = _p10._0;
			var intermediateResult = _p10._1;
			return A3(
				_elm_lang$core$List$foldl,
				F2(
					function (_p11, result) {
						var _p12 = _p11;
						return A3(leftStep, _p12._0, _p12._1, result);
					}),
				intermediateResult,
				leftovers);
		});
	var _elm_lang$core$Dict$reportRemBug = F4(
		function (msg, c, lgot, rgot) {
			return _elm_lang$core$Native_Debug.crash(
				_elm_lang$core$String$concat(
					_elm_lang$core$Native_List.fromArray(
						[
							'Internal red-black tree invariant violated, expected ',
							msg,
							' and got ',
							_elm_lang$core$Basics$toString(c),
							'/',
							lgot,
							'/',
							rgot,
							'\nPlease report this bug to <https://github.com/elm-lang/core/issues>'
						])));
		});
	var _elm_lang$core$Dict$isBBlack = function (dict) {
		var _p13 = dict;
		_v11_2:
		do {
			if (_p13.ctor === 'RBNode_elm_builtin') {
				if (_p13._0.ctor === 'BBlack') {
					return true;
				} else {
					break _v11_2;
				}
			} else {
				if (_p13._0.ctor === 'LBBlack') {
					return true;
				} else {
					break _v11_2;
				}
			}
		} while(false);
		return false;
	};
	var _elm_lang$core$Dict$sizeHelp = F2(
		function (n, dict) {
			sizeHelp:
			while (true) {
				var _p14 = dict;
				if (_p14.ctor === 'RBEmpty_elm_builtin') {
					return n;
				} else {
					var _v13 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
						_v14 = _p14._3;
					n = _v13;
					dict = _v14;
					continue sizeHelp;
				}
			}
		});
	var _elm_lang$core$Dict$size = function (dict) {
		return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
	};
	var _elm_lang$core$Dict$get = F2(
		function (targetKey, dict) {
			get:
			while (true) {
				var _p15 = dict;
				if (_p15.ctor === 'RBEmpty_elm_builtin') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
					switch (_p16.ctor) {
						case 'LT':
							var _v17 = targetKey,
								_v18 = _p15._3;
							targetKey = _v17;
							dict = _v18;
							continue get;
						case 'EQ':
							return _elm_lang$core$Maybe$Just(_p15._2);
						default:
							var _v19 = targetKey,
								_v20 = _p15._4;
							targetKey = _v19;
							dict = _v20;
							continue get;
					}
				}
			}
		});
	var _elm_lang$core$Dict$member = F2(
		function (key, dict) {
			var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
			if (_p17.ctor === 'Just') {
				return true;
			} else {
				return false;
			}
		});
	var _elm_lang$core$Dict$maxWithDefault = F3(
		function (k, v, r) {
			maxWithDefault:
			while (true) {
				var _p18 = r;
				if (_p18.ctor === 'RBEmpty_elm_builtin') {
					return {ctor: '_Tuple2', _0: k, _1: v};
				} else {
					var _v23 = _p18._1,
						_v24 = _p18._2,
						_v25 = _p18._4;
					k = _v23;
					v = _v24;
					r = _v25;
					continue maxWithDefault;
				}
			}
		});
	var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
	var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
	var _elm_lang$core$Dict$Black = {ctor: 'Black'};
	var _elm_lang$core$Dict$blackish = function (t) {
		var _p19 = t;
		if (_p19.ctor === 'RBNode_elm_builtin') {
			var _p20 = _p19._0;
			return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
		} else {
			return true;
		}
	};
	var _elm_lang$core$Dict$Red = {ctor: 'Red'};
	var _elm_lang$core$Dict$moreBlack = function (color) {
		var _p21 = color;
		switch (_p21.ctor) {
			case 'Black':
				return _elm_lang$core$Dict$BBlack;
			case 'Red':
				return _elm_lang$core$Dict$Black;
			case 'NBlack':
				return _elm_lang$core$Dict$Red;
			default:
				return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
		}
	};
	var _elm_lang$core$Dict$lessBlack = function (color) {
		var _p22 = color;
		switch (_p22.ctor) {
			case 'BBlack':
				return _elm_lang$core$Dict$Black;
			case 'Black':
				return _elm_lang$core$Dict$Red;
			case 'Red':
				return _elm_lang$core$Dict$NBlack;
			default:
				return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
		}
	};
	var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
	var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
	var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
		return {ctor: 'RBEmpty_elm_builtin', _0: a};
	};
	var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	var _elm_lang$core$Dict$isEmpty = function (dict) {
		return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
	};
	var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
		function (a, b, c, d, e) {
			return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
		});
	var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
		var _p23 = dict;
		if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
			return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
		} else {
			return dict;
		}
	};
	var _elm_lang$core$Dict$lessBlackTree = function (dict) {
		var _p24 = dict;
		if (_p24.ctor === 'RBNode_elm_builtin') {
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$lessBlack(_p24._0),
				_p24._1,
				_p24._2,
				_p24._3,
				_p24._4);
		} else {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		}
	};
	var _elm_lang$core$Dict$balancedTree = function (col) {
		return function (xk) {
			return function (xv) {
				return function (yk) {
					return function (yv) {
						return function (zk) {
							return function (zv) {
								return function (a) {
									return function (b) {
										return function (c) {
											return function (d) {
												return A5(
													_elm_lang$core$Dict$RBNode_elm_builtin,
													_elm_lang$core$Dict$lessBlack(col),
													yk,
													yv,
													A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
													A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
	var _elm_lang$core$Dict$blacken = function (t) {
		var _p25 = t;
		if (_p25.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
		}
	};
	var _elm_lang$core$Dict$redden = function (t) {
		var _p26 = t;
		if (_p26.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
		} else {
			return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
		}
	};
	var _elm_lang$core$Dict$balanceHelp = function (tree) {
		var _p27 = tree;
		_v33_6:
		do {
			_v33_5:
			do {
				_v33_4:
				do {
					_v33_3:
					do {
						_v33_2:
						do {
							_v33_1:
							do {
								_v33_0:
								do {
									if (_p27.ctor === 'RBNode_elm_builtin') {
										if (_p27._3.ctor === 'RBNode_elm_builtin') {
											if (_p27._4.ctor === 'RBNode_elm_builtin') {
												switch (_p27._3._0.ctor) {
													case 'Red':
														switch (_p27._4._0.ctor) {
															case 'Red':
																if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																	break _v33_0;
																} else {
																	if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																		break _v33_1;
																	} else {
																		if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																			break _v33_2;
																		} else {
																			if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																				break _v33_3;
																			} else {
																				break _v33_6;
																			}
																		}
																	}
																}
															case 'NBlack':
																if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																	break _v33_0;
																} else {
																	if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																		break _v33_1;
																	} else {
																		if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																			break _v33_4;
																		} else {
																			break _v33_6;
																		}
																	}
																}
															default:
																if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																	break _v33_0;
																} else {
																	if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																		break _v33_1;
																	} else {
																		break _v33_6;
																	}
																}
														}
													case 'NBlack':
														switch (_p27._4._0.ctor) {
															case 'Red':
																if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																	break _v33_2;
																} else {
																	if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																		break _v33_3;
																	} else {
																		if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																			break _v33_5;
																		} else {
																			break _v33_6;
																		}
																	}
																}
															case 'NBlack':
																if (_p27._0.ctor === 'BBlack') {
																	if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v33_4;
																	} else {
																		if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																			break _v33_5;
																		} else {
																			break _v33_6;
																		}
																	}
																} else {
																	break _v33_6;
																}
															default:
																if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																	break _v33_5;
																} else {
																	break _v33_6;
																}
														}
													default:
														switch (_p27._4._0.ctor) {
															case 'Red':
																if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																	break _v33_2;
																} else {
																	if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																		break _v33_3;
																	} else {
																		break _v33_6;
																	}
																}
															case 'NBlack':
																if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v33_4;
																} else {
																	break _v33_6;
																}
															default:
																break _v33_6;
														}
												}
											} else {
												switch (_p27._3._0.ctor) {
													case 'Red':
														if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
															break _v33_0;
														} else {
															if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																break _v33_1;
															} else {
																break _v33_6;
															}
														}
													case 'NBlack':
														if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
															break _v33_5;
														} else {
															break _v33_6;
														}
													default:
														break _v33_6;
												}
											}
										} else {
											if (_p27._4.ctor === 'RBNode_elm_builtin') {
												switch (_p27._4._0.ctor) {
													case 'Red':
														if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
															break _v33_2;
														} else {
															if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																break _v33_3;
															} else {
																break _v33_6;
															}
														}
													case 'NBlack':
														if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
															break _v33_4;
														} else {
															break _v33_6;
														}
													default:
														break _v33_6;
												}
											} else {
												break _v33_6;
											}
										}
									} else {
										break _v33_6;
									}
								} while(false);
								return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
				} while(false);
				return A5(
					_elm_lang$core$Dict$RBNode_elm_builtin,
					_elm_lang$core$Dict$Black,
					_p27._4._3._1,
					_p27._4._3._2,
					A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
					A5(
						_elm_lang$core$Dict$balance,
						_elm_lang$core$Dict$Black,
						_p27._4._1,
						_p27._4._2,
						_p27._4._3._4,
						_elm_lang$core$Dict$redden(_p27._4._4)));
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._3._4._1,
				_p27._3._4._2,
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._3._1,
					_p27._3._2,
					_elm_lang$core$Dict$redden(_p27._3._3),
					_p27._3._4._3),
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
		} while(false);
		return tree;
	};
	var _elm_lang$core$Dict$balance = F5(
		function (c, k, v, l, r) {
			var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
			return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
		});
	var _elm_lang$core$Dict$bubble = F5(
		function (c, k, v, l, r) {
			return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$moreBlack(c),
				k,
				v,
				_elm_lang$core$Dict$lessBlackTree(l),
				_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		});
	var _elm_lang$core$Dict$removeMax = F5(
		function (c, k, v, l, r) {
			var _p28 = r;
			if (_p28.ctor === 'RBEmpty_elm_builtin') {
				return A3(_elm_lang$core$Dict$rem, c, l, r);
			} else {
				return A5(
					_elm_lang$core$Dict$bubble,
					c,
					k,
					v,
					l,
					A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
			}
		});
	var _elm_lang$core$Dict$rem = F3(
		function (c, l, r) {
			var _p29 = {ctor: '_Tuple2', _0: l, _1: r};
			if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
				if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
					var _p30 = c;
					switch (_p30.ctor) {
						case 'Red':
							return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
						case 'Black':
							return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
						default:
							return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
					}
				} else {
					var _p33 = _p29._1._0;
					var _p32 = _p29._0._0;
					var _p31 = {ctor: '_Tuple3', _0: c, _1: _p32, _2: _p33};
					if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
						return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
					} else {
						return A4(
							_elm_lang$core$Dict$reportRemBug,
							'Black/LBlack/Red',
							c,
							_elm_lang$core$Basics$toString(_p32),
							_elm_lang$core$Basics$toString(_p33));
					}
				}
			} else {
				if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
					var _p36 = _p29._1._0;
					var _p35 = _p29._0._0;
					var _p34 = {ctor: '_Tuple3', _0: c, _1: _p35, _2: _p36};
					if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
						return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
					} else {
						return A4(
							_elm_lang$core$Dict$reportRemBug,
							'Black/Red/LBlack',
							c,
							_elm_lang$core$Basics$toString(_p35),
							_elm_lang$core$Basics$toString(_p36));
					}
				} else {
					var _p40 = _p29._0._2;
					var _p39 = _p29._0._4;
					var _p38 = _p29._0._1;
					var l$ = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
					var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
					var k = _p37._0;
					var v = _p37._1;
					return A5(_elm_lang$core$Dict$bubble, c, k, v, l$, r);
				}
			}
		});
	var _elm_lang$core$Dict$map = F2(
		function (f, dict) {
			var _p41 = dict;
			if (_p41.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
			} else {
				var _p42 = _p41._1;
				return A5(
					_elm_lang$core$Dict$RBNode_elm_builtin,
					_p41._0,
					_p42,
					A2(f, _p42, _p41._2),
					A2(_elm_lang$core$Dict$map, f, _p41._3),
					A2(_elm_lang$core$Dict$map, f, _p41._4));
			}
		});
	var _elm_lang$core$Dict$Same = {ctor: 'Same'};
	var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
	var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
	var _elm_lang$core$Dict$update = F3(
		function (k, alter, dict) {
			var up = function (dict) {
				var _p43 = dict;
				if (_p43.ctor === 'RBEmpty_elm_builtin') {
					var _p44 = alter(_elm_lang$core$Maybe$Nothing);
					if (_p44.ctor === 'Nothing') {
						return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
					} else {
						return {
							ctor: '_Tuple2',
							_0: _elm_lang$core$Dict$Insert,
							_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
						};
					}
				} else {
					var _p55 = _p43._2;
					var _p54 = _p43._4;
					var _p53 = _p43._3;
					var _p52 = _p43._1;
					var _p51 = _p43._0;
					var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
					switch (_p45.ctor) {
						case 'EQ':
							var _p46 = alter(
								_elm_lang$core$Maybe$Just(_p55));
							if (_p46.ctor === 'Nothing') {
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
								};
							}
						case 'LT':
							var _p47 = up(_p53);
							var flag = _p47._0;
							var newLeft = _p47._1;
							var _p48 = flag;
							switch (_p48.ctor) {
								case 'Same':
									return {
										ctor: '_Tuple2',
										_0: _elm_lang$core$Dict$Same,
										_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
									};
								case 'Insert':
									return {
										ctor: '_Tuple2',
										_0: _elm_lang$core$Dict$Insert,
										_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
									};
								default:
									return {
										ctor: '_Tuple2',
										_0: _elm_lang$core$Dict$Remove,
										_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
									};
							}
						default:
							var _p49 = up(_p54);
							var flag = _p49._0;
							var newRight = _p49._1;
							var _p50 = flag;
							switch (_p50.ctor) {
								case 'Same':
									return {
										ctor: '_Tuple2',
										_0: _elm_lang$core$Dict$Same,
										_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
									};
								case 'Insert':
									return {
										ctor: '_Tuple2',
										_0: _elm_lang$core$Dict$Insert,
										_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
									};
								default:
									return {
										ctor: '_Tuple2',
										_0: _elm_lang$core$Dict$Remove,
										_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
									};
							}
					}
				}
			};
			var _p56 = up(dict);
			var flag = _p56._0;
			var updatedDict = _p56._1;
			var _p57 = flag;
			switch (_p57.ctor) {
				case 'Same':
					return updatedDict;
				case 'Insert':
					return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
				default:
					return _elm_lang$core$Dict$blacken(updatedDict);
			}
		});
	var _elm_lang$core$Dict$insert = F3(
		function (key, value, dict) {
			return A3(
				_elm_lang$core$Dict$update,
				key,
				_elm_lang$core$Basics$always(
					_elm_lang$core$Maybe$Just(value)),
				dict);
		});
	var _elm_lang$core$Dict$singleton = F2(
		function (key, value) {
			return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
		});
	var _elm_lang$core$Dict$union = F2(
		function (t1, t2) {
			return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
		});
	var _elm_lang$core$Dict$filter = F2(
		function (predicate, dictionary) {
			var add = F3(
				function (key, value, dict) {
					return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
				});
			return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
		});
	var _elm_lang$core$Dict$intersect = F2(
		function (t1, t2) {
			return A2(
				_elm_lang$core$Dict$filter,
				F2(
					function (k, _p58) {
						return A2(_elm_lang$core$Dict$member, k, t2);
					}),
				t1);
		});
	var _elm_lang$core$Dict$partition = F2(
		function (predicate, dict) {
			var add = F3(
				function (key, value, _p59) {
					var _p60 = _p59;
					var _p62 = _p60._1;
					var _p61 = _p60._0;
					return A2(predicate, key, value) ? {
						ctor: '_Tuple2',
						_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
						_1: _p62
					} : {
						ctor: '_Tuple2',
						_0: _p61,
						_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
					};
				});
			return A3(
				_elm_lang$core$Dict$foldl,
				add,
				{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
				dict);
		});
	var _elm_lang$core$Dict$fromList = function (assocs) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p63, dict) {
					var _p64 = _p63;
					return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
				}),
			_elm_lang$core$Dict$empty,
			assocs);
	};
	var _elm_lang$core$Dict$remove = F2(
		function (key, dict) {
			return A3(
				_elm_lang$core$Dict$update,
				key,
				_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
				dict);
		});
	var _elm_lang$core$Dict$diff = F2(
		function (t1, t2) {
			return A3(
				_elm_lang$core$Dict$foldl,
				F3(
					function (k, v, t) {
						return A2(_elm_lang$core$Dict$remove, k, t);
					}),
				t1,
				t2);
		});
	
	//import Native.Scheduler //
	
	var _elm_lang$core$Native_Time = function() {
	
	var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
	});
	
	function setInterval_(interval, task)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
		{
			var id = setInterval(function() {
				_elm_lang$core$Native_Scheduler.rawSpawn(task);
			}, interval);
	
			return function() { clearInterval(id); };
		});
	}
	
	return {
		now: now,
		setInterval_: F2(setInterval_)
	};
	
	}();
	var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
	var _elm_lang$core$Time$spawnHelp = F3(
		function (router, intervals, processes) {
			var _p0 = intervals;
			if (_p0.ctor === '[]') {
				return _elm_lang$core$Task$succeed(processes);
			} else {
				var _p1 = _p0._0;
				return A2(
					_elm_lang$core$Task$andThen,
					_elm_lang$core$Native_Scheduler.spawn(
						A2(
							_elm_lang$core$Time$setInterval,
							_p1,
							A2(_elm_lang$core$Platform$sendToSelf, router, _p1))),
					function (id) {
						return A3(
							_elm_lang$core$Time$spawnHelp,
							router,
							_p0._1,
							A3(_elm_lang$core$Dict$insert, _p1, id, processes));
					});
			}
		});
	var _elm_lang$core$Time$addMySub = F2(
		function (_p2, state) {
			var _p3 = _p2;
			var _p6 = _p3._1;
			var _p5 = _p3._0;
			var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
			if (_p4.ctor === 'Nothing') {
				return A3(
					_elm_lang$core$Dict$insert,
					_p5,
					_elm_lang$core$Native_List.fromArray(
						[_p6]),
					state);
			} else {
				return A3(
					_elm_lang$core$Dict$insert,
					_p5,
					A2(_elm_lang$core$List_ops['::'], _p6, _p4._0),
					state);
			}
		});
	var _elm_lang$core$Time$inMilliseconds = function (t) {
		return t;
	};
	var _elm_lang$core$Time$millisecond = 1;
	var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
	var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
	var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
	var _elm_lang$core$Time$inHours = function (t) {
		return t / _elm_lang$core$Time$hour;
	};
	var _elm_lang$core$Time$inMinutes = function (t) {
		return t / _elm_lang$core$Time$minute;
	};
	var _elm_lang$core$Time$inSeconds = function (t) {
		return t / _elm_lang$core$Time$second;
	};
	var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
	var _elm_lang$core$Time$onSelfMsg = F3(
		function (router, interval, state) {
			var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
			if (_p7.ctor === 'Nothing') {
				return _elm_lang$core$Task$succeed(state);
			} else {
				return A2(
					_elm_lang$core$Task$andThen,
					_elm_lang$core$Time$now,
					function (time) {
						return A2(
							_elm_lang$core$Task$andThen,
							_elm_lang$core$Task$sequence(
								A2(
									_elm_lang$core$List$map,
									function (tagger) {
										return A2(
											_elm_lang$core$Platform$sendToApp,
											router,
											tagger(time));
									},
									_p7._0)),
							function (_p8) {
								return _elm_lang$core$Task$succeed(state);
							});
					});
			}
		});
	var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
	var _elm_lang$core$Time$State = F2(
		function (a, b) {
			return {taggers: a, processes: b};
		});
	var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
		A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
	var _elm_lang$core$Time$onEffects = F3(
		function (router, subs, _p9) {
			var _p10 = _p9;
			var rightStep = F3(
				function (_p12, id, _p11) {
					var _p13 = _p11;
					return {
						ctor: '_Tuple3',
						_0: _p13._0,
						_1: _p13._1,
						_2: A2(
							_elm_lang$core$Task$andThen,
							_elm_lang$core$Native_Scheduler.kill(id),
							function (_p14) {
								return _p13._2;
							})
					};
				});
			var bothStep = F4(
				function (interval, taggers, id, _p15) {
					var _p16 = _p15;
					return {
						ctor: '_Tuple3',
						_0: _p16._0,
						_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
						_2: _p16._2
					};
				});
			var leftStep = F3(
				function (interval, taggers, _p17) {
					var _p18 = _p17;
					return {
						ctor: '_Tuple3',
						_0: A2(_elm_lang$core$List_ops['::'], interval, _p18._0),
						_1: _p18._1,
						_2: _p18._2
					};
				});
			var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
			var _p19 = A6(
				_elm_lang$core$Dict$merge,
				leftStep,
				bothStep,
				rightStep,
				newTaggers,
				_p10.processes,
				{
					ctor: '_Tuple3',
					_0: _elm_lang$core$Native_List.fromArray(
						[]),
					_1: _elm_lang$core$Dict$empty,
					_2: _elm_lang$core$Task$succeed(
						{ctor: '_Tuple0'})
				});
			var spawnList = _p19._0;
			var existingDict = _p19._1;
			var killTask = _p19._2;
			return A2(
				_elm_lang$core$Task$andThen,
				killTask,
				function (_p20) {
					return A2(
						_elm_lang$core$Task$andThen,
						A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict),
						function (newProcesses) {
							return _elm_lang$core$Task$succeed(
								A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
						});
				});
		});
	var _elm_lang$core$Time$Every = F2(
		function (a, b) {
			return {ctor: 'Every', _0: a, _1: b};
		});
	var _elm_lang$core$Time$every = F2(
		function (interval, tagger) {
			return _elm_lang$core$Time$subscription(
				A2(_elm_lang$core$Time$Every, interval, tagger));
		});
	var _elm_lang$core$Time$subMap = F2(
		function (f, _p21) {
			var _p22 = _p21;
			return A2(
				_elm_lang$core$Time$Every,
				_p22._0,
				function (_p23) {
					return f(
						_p22._1(_p23));
				});
		});
	_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};
	
	var _elm_lang$core$Random$onSelfMsg = F3(
		function (_p1, _p0, seed) {
			return _elm_lang$core$Task$succeed(seed);
		});
	var _elm_lang$core$Random$magicNum8 = 2147483562;
	var _elm_lang$core$Random$range = function (_p2) {
		return {ctor: '_Tuple2', _0: 0, _1: _elm_lang$core$Random$magicNum8};
	};
	var _elm_lang$core$Random$magicNum7 = 2137383399;
	var _elm_lang$core$Random$magicNum6 = 2147483563;
	var _elm_lang$core$Random$magicNum5 = 3791;
	var _elm_lang$core$Random$magicNum4 = 40692;
	var _elm_lang$core$Random$magicNum3 = 52774;
	var _elm_lang$core$Random$magicNum2 = 12211;
	var _elm_lang$core$Random$magicNum1 = 53668;
	var _elm_lang$core$Random$magicNum0 = 40014;
	var _elm_lang$core$Random$step = F2(
		function (_p3, seed) {
			var _p4 = _p3;
			return _p4._0(seed);
		});
	var _elm_lang$core$Random$onEffects = F3(
		function (router, commands, seed) {
			var _p5 = commands;
			if (_p5.ctor === '[]') {
				return _elm_lang$core$Task$succeed(seed);
			} else {
				var _p6 = A2(_elm_lang$core$Random$step, _p5._0._0, seed);
				var value = _p6._0;
				var newSeed = _p6._1;
				return A2(
					_elm_lang$core$Task$andThen,
					A2(_elm_lang$core$Platform$sendToApp, router, value),
					function (_p7) {
						return A3(_elm_lang$core$Random$onEffects, router, _p5._1, newSeed);
					});
			}
		});
	var _elm_lang$core$Random$listHelp = F4(
		function (list, n, generate, seed) {
			listHelp:
			while (true) {
				if (_elm_lang$core$Native_Utils.cmp(n, 1) < 0) {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$List$reverse(list),
						_1: seed
					};
				} else {
					var _p8 = generate(seed);
					var value = _p8._0;
					var newSeed = _p8._1;
					var _v2 = A2(_elm_lang$core$List_ops['::'], value, list),
						_v3 = n - 1,
						_v4 = generate,
						_v5 = newSeed;
					list = _v2;
					n = _v3;
					generate = _v4;
					seed = _v5;
					continue listHelp;
				}
			}
		});
	var _elm_lang$core$Random$minInt = -2147483648;
	var _elm_lang$core$Random$maxInt = 2147483647;
	var _elm_lang$core$Random$iLogBase = F2(
		function (b, i) {
			return (_elm_lang$core$Native_Utils.cmp(i, b) < 0) ? 1 : (1 + A2(_elm_lang$core$Random$iLogBase, b, (i / b) | 0));
		});
	var _elm_lang$core$Random$command = _elm_lang$core$Native_Platform.leaf('Random');
	var _elm_lang$core$Random$Generator = function (a) {
		return {ctor: 'Generator', _0: a};
	};
	var _elm_lang$core$Random$list = F2(
		function (n, _p9) {
			var _p10 = _p9;
			return _elm_lang$core$Random$Generator(
				function (seed) {
					return A4(
						_elm_lang$core$Random$listHelp,
						_elm_lang$core$Native_List.fromArray(
							[]),
						n,
						_p10._0,
						seed);
				});
		});
	var _elm_lang$core$Random$map = F2(
		function (func, _p11) {
			var _p12 = _p11;
			return _elm_lang$core$Random$Generator(
				function (seed0) {
					var _p13 = _p12._0(seed0);
					var a = _p13._0;
					var seed1 = _p13._1;
					return {
						ctor: '_Tuple2',
						_0: func(a),
						_1: seed1
					};
				});
		});
	var _elm_lang$core$Random$map2 = F3(
		function (func, _p15, _p14) {
			var _p16 = _p15;
			var _p17 = _p14;
			return _elm_lang$core$Random$Generator(
				function (seed0) {
					var _p18 = _p16._0(seed0);
					var a = _p18._0;
					var seed1 = _p18._1;
					var _p19 = _p17._0(seed1);
					var b = _p19._0;
					var seed2 = _p19._1;
					return {
						ctor: '_Tuple2',
						_0: A2(func, a, b),
						_1: seed2
					};
				});
		});
	var _elm_lang$core$Random$pair = F2(
		function (genA, genB) {
			return A3(
				_elm_lang$core$Random$map2,
				F2(
					function (v0, v1) {
						return {ctor: '_Tuple2', _0: v0, _1: v1};
					}),
				genA,
				genB);
		});
	var _elm_lang$core$Random$map3 = F4(
		function (func, _p22, _p21, _p20) {
			var _p23 = _p22;
			var _p24 = _p21;
			var _p25 = _p20;
			return _elm_lang$core$Random$Generator(
				function (seed0) {
					var _p26 = _p23._0(seed0);
					var a = _p26._0;
					var seed1 = _p26._1;
					var _p27 = _p24._0(seed1);
					var b = _p27._0;
					var seed2 = _p27._1;
					var _p28 = _p25._0(seed2);
					var c = _p28._0;
					var seed3 = _p28._1;
					return {
						ctor: '_Tuple2',
						_0: A3(func, a, b, c),
						_1: seed3
					};
				});
		});
	var _elm_lang$core$Random$map4 = F5(
		function (func, _p32, _p31, _p30, _p29) {
			var _p33 = _p32;
			var _p34 = _p31;
			var _p35 = _p30;
			var _p36 = _p29;
			return _elm_lang$core$Random$Generator(
				function (seed0) {
					var _p37 = _p33._0(seed0);
					var a = _p37._0;
					var seed1 = _p37._1;
					var _p38 = _p34._0(seed1);
					var b = _p38._0;
					var seed2 = _p38._1;
					var _p39 = _p35._0(seed2);
					var c = _p39._0;
					var seed3 = _p39._1;
					var _p40 = _p36._0(seed3);
					var d = _p40._0;
					var seed4 = _p40._1;
					return {
						ctor: '_Tuple2',
						_0: A4(func, a, b, c, d),
						_1: seed4
					};
				});
		});
	var _elm_lang$core$Random$map5 = F6(
		function (func, _p45, _p44, _p43, _p42, _p41) {
			var _p46 = _p45;
			var _p47 = _p44;
			var _p48 = _p43;
			var _p49 = _p42;
			var _p50 = _p41;
			return _elm_lang$core$Random$Generator(
				function (seed0) {
					var _p51 = _p46._0(seed0);
					var a = _p51._0;
					var seed1 = _p51._1;
					var _p52 = _p47._0(seed1);
					var b = _p52._0;
					var seed2 = _p52._1;
					var _p53 = _p48._0(seed2);
					var c = _p53._0;
					var seed3 = _p53._1;
					var _p54 = _p49._0(seed3);
					var d = _p54._0;
					var seed4 = _p54._1;
					var _p55 = _p50._0(seed4);
					var e = _p55._0;
					var seed5 = _p55._1;
					return {
						ctor: '_Tuple2',
						_0: A5(func, a, b, c, d, e),
						_1: seed5
					};
				});
		});
	var _elm_lang$core$Random$andThen = F2(
		function (_p56, callback) {
			var _p57 = _p56;
			return _elm_lang$core$Random$Generator(
				function (seed) {
					var _p58 = _p57._0(seed);
					var result = _p58._0;
					var newSeed = _p58._1;
					var _p59 = callback(result);
					var genB = _p59._0;
					return genB(newSeed);
				});
		});
	var _elm_lang$core$Random$State = F2(
		function (a, b) {
			return {ctor: 'State', _0: a, _1: b};
		});
	var _elm_lang$core$Random$initState = function (s$) {
		var s = A2(_elm_lang$core$Basics$max, s$, 0 - s$);
		var q = (s / (_elm_lang$core$Random$magicNum6 - 1)) | 0;
		var s2 = A2(_elm_lang$core$Basics_ops['%'], q, _elm_lang$core$Random$magicNum7 - 1);
		var s1 = A2(_elm_lang$core$Basics_ops['%'], s, _elm_lang$core$Random$magicNum6 - 1);
		return A2(_elm_lang$core$Random$State, s1 + 1, s2 + 1);
	};
	var _elm_lang$core$Random$next = function (_p60) {
		var _p61 = _p60;
		var _p63 = _p61._1;
		var _p62 = _p61._0;
		var k$ = (_p63 / _elm_lang$core$Random$magicNum3) | 0;
		var s2$ = (_elm_lang$core$Random$magicNum4 * (_p63 - (k$ * _elm_lang$core$Random$magicNum3))) - (k$ * _elm_lang$core$Random$magicNum5);
		var s2$$ = (_elm_lang$core$Native_Utils.cmp(s2$, 0) < 0) ? (s2$ + _elm_lang$core$Random$magicNum7) : s2$;
		var k = (_p62 / _elm_lang$core$Random$magicNum1) | 0;
		var s1$ = (_elm_lang$core$Random$magicNum0 * (_p62 - (k * _elm_lang$core$Random$magicNum1))) - (k * _elm_lang$core$Random$magicNum2);
		var s1$$ = (_elm_lang$core$Native_Utils.cmp(s1$, 0) < 0) ? (s1$ + _elm_lang$core$Random$magicNum6) : s1$;
		var z = s1$$ - s2$$;
		var z$ = (_elm_lang$core$Native_Utils.cmp(z, 1) < 0) ? (z + _elm_lang$core$Random$magicNum8) : z;
		return {
			ctor: '_Tuple2',
			_0: z$,
			_1: A2(_elm_lang$core$Random$State, s1$$, s2$$)
		};
	};
	var _elm_lang$core$Random$split = function (_p64) {
		var _p65 = _p64;
		var _p68 = _p65._1;
		var _p67 = _p65._0;
		var _p66 = _elm_lang$core$Basics$snd(
			_elm_lang$core$Random$next(_p65));
		var t1 = _p66._0;
		var t2 = _p66._1;
		var new_s2 = _elm_lang$core$Native_Utils.eq(_p68, 1) ? (_elm_lang$core$Random$magicNum7 - 1) : (_p68 - 1);
		var new_s1 = _elm_lang$core$Native_Utils.eq(_p67, _elm_lang$core$Random$magicNum6 - 1) ? 1 : (_p67 + 1);
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_lang$core$Random$State, new_s1, t2),
			_1: A2(_elm_lang$core$Random$State, t1, new_s2)
		};
	};
	var _elm_lang$core$Random$Seed = function (a) {
		return {ctor: 'Seed', _0: a};
	};
	var _elm_lang$core$Random$int = F2(
		function (a, b) {
			return _elm_lang$core$Random$Generator(
				function (_p69) {
					var _p70 = _p69;
					var _p75 = _p70._0;
					var base = 2147483561;
					var f = F3(
						function (n, acc, state) {
							f:
							while (true) {
								var _p71 = n;
								if (_p71 === 0) {
									return {ctor: '_Tuple2', _0: acc, _1: state};
								} else {
									var _p72 = _p75.next(state);
									var x = _p72._0;
									var state$ = _p72._1;
									var _v27 = n - 1,
										_v28 = x + (acc * base),
										_v29 = state$;
									n = _v27;
									acc = _v28;
									state = _v29;
									continue f;
								}
							}
						});
					var _p73 = (_elm_lang$core$Native_Utils.cmp(a, b) < 0) ? {ctor: '_Tuple2', _0: a, _1: b} : {ctor: '_Tuple2', _0: b, _1: a};
					var lo = _p73._0;
					var hi = _p73._1;
					var k = (hi - lo) + 1;
					var n = A2(_elm_lang$core$Random$iLogBase, base, k);
					var _p74 = A3(f, n, 1, _p75.state);
					var v = _p74._0;
					var state$ = _p74._1;
					return {
						ctor: '_Tuple2',
						_0: lo + A2(_elm_lang$core$Basics_ops['%'], v, k),
						_1: _elm_lang$core$Random$Seed(
							_elm_lang$core$Native_Utils.update(
								_p75,
								{state: state$}))
					};
				});
		});
	var _elm_lang$core$Random$bool = A2(
		_elm_lang$core$Random$map,
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(1),
		A2(_elm_lang$core$Random$int, 0, 1));
	var _elm_lang$core$Random$float = F2(
		function (a, b) {
			return _elm_lang$core$Random$Generator(
				function (seed) {
					var _p76 = A2(
						_elm_lang$core$Random$step,
						A2(_elm_lang$core$Random$int, _elm_lang$core$Random$minInt, _elm_lang$core$Random$maxInt),
						seed);
					var number = _p76._0;
					var newSeed = _p76._1;
					var negativeOneToOne = _elm_lang$core$Basics$toFloat(number) / _elm_lang$core$Basics$toFloat(_elm_lang$core$Random$maxInt - _elm_lang$core$Random$minInt);
					var _p77 = (_elm_lang$core$Native_Utils.cmp(a, b) < 0) ? {ctor: '_Tuple2', _0: a, _1: b} : {ctor: '_Tuple2', _0: b, _1: a};
					var lo = _p77._0;
					var hi = _p77._1;
					var scaled = ((lo + hi) / 2) + ((hi - lo) * negativeOneToOne);
					return {ctor: '_Tuple2', _0: scaled, _1: newSeed};
				});
		});
	var _elm_lang$core$Random$initialSeed = function (n) {
		return _elm_lang$core$Random$Seed(
			{
				state: _elm_lang$core$Random$initState(n),
				next: _elm_lang$core$Random$next,
				split: _elm_lang$core$Random$split,
				range: _elm_lang$core$Random$range
			});
	};
	var _elm_lang$core$Random$init = A2(
		_elm_lang$core$Task$andThen,
		_elm_lang$core$Time$now,
		function (t) {
			return _elm_lang$core$Task$succeed(
				_elm_lang$core$Random$initialSeed(
					_elm_lang$core$Basics$round(t)));
		});
	var _elm_lang$core$Random$Generate = function (a) {
		return {ctor: 'Generate', _0: a};
	};
	var _elm_lang$core$Random$generate = F2(
		function (tagger, generator) {
			return _elm_lang$core$Random$command(
				_elm_lang$core$Random$Generate(
					A2(_elm_lang$core$Random$map, tagger, generator)));
		});
	var _elm_lang$core$Random$cmdMap = F2(
		function (func, _p78) {
			var _p79 = _p78;
			return _elm_lang$core$Random$Generate(
				A2(_elm_lang$core$Random$map, func, _p79._0));
		});
	_elm_lang$core$Native_Platform.effectManagers['Random'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Random$init, onEffects: _elm_lang$core$Random$onEffects, onSelfMsg: _elm_lang$core$Random$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Random$cmdMap};
	
	var _elm_lang$lazy$Native_Lazy = function() {
	
	function memoize(thunk)
	{
	    var value;
	    var isForced = false;
	    return function(tuple0) {
	        if (!isForced) {
	            value = thunk(tuple0);
	            isForced = true;
	        }
	        return value;
	    };
	}
	
	return {
	    memoize: memoize
	};
	
	}();
	
	var _elm_lang$lazy$Lazy$force = function (_p0) {
		var _p1 = _p0;
		return _p1._0(
			{ctor: '_Tuple0'});
	};
	var _elm_lang$lazy$Lazy$Lazy = function (a) {
		return {ctor: 'Lazy', _0: a};
	};
	var _elm_lang$lazy$Lazy$lazy = function (thunk) {
		return _elm_lang$lazy$Lazy$Lazy(
			_elm_lang$lazy$Native_Lazy.memoize(thunk));
	};
	var _elm_lang$lazy$Lazy$map = F2(
		function (f, a) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p2) {
					var _p3 = _p2;
					return f(
						_elm_lang$lazy$Lazy$force(a));
				});
		});
	var _elm_lang$lazy$Lazy$map2 = F3(
		function (f, a, b) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p4) {
					var _p5 = _p4;
					return A2(
						f,
						_elm_lang$lazy$Lazy$force(a),
						_elm_lang$lazy$Lazy$force(b));
				});
		});
	var _elm_lang$lazy$Lazy$map3 = F4(
		function (f, a, b, c) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p6) {
					var _p7 = _p6;
					return A3(
						f,
						_elm_lang$lazy$Lazy$force(a),
						_elm_lang$lazy$Lazy$force(b),
						_elm_lang$lazy$Lazy$force(c));
				});
		});
	var _elm_lang$lazy$Lazy$map4 = F5(
		function (f, a, b, c, d) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p8) {
					var _p9 = _p8;
					return A4(
						f,
						_elm_lang$lazy$Lazy$force(a),
						_elm_lang$lazy$Lazy$force(b),
						_elm_lang$lazy$Lazy$force(c),
						_elm_lang$lazy$Lazy$force(d));
				});
		});
	var _elm_lang$lazy$Lazy$map5 = F6(
		function (f, a, b, c, d, e) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p10) {
					var _p11 = _p10;
					return A5(
						f,
						_elm_lang$lazy$Lazy$force(a),
						_elm_lang$lazy$Lazy$force(b),
						_elm_lang$lazy$Lazy$force(c),
						_elm_lang$lazy$Lazy$force(d),
						_elm_lang$lazy$Lazy$force(e));
				});
		});
	var _elm_lang$lazy$Lazy$apply = F2(
		function (f, x) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p12) {
					var _p13 = _p12;
					return A2(
						_elm_lang$lazy$Lazy$force,
						f,
						_elm_lang$lazy$Lazy$force(x));
				});
		});
	var _elm_lang$lazy$Lazy$andThen = F2(
		function (a, callback) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p14) {
					var _p15 = _p14;
					return _elm_lang$lazy$Lazy$force(
						callback(
							_elm_lang$lazy$Lazy$force(a)));
				});
		});
	
	var _elm_community$elm_lazy_list$Lazy_List$toArray = function (list) {
		var _p0 = _elm_lang$lazy$Lazy$force(list);
		if (_p0.ctor === 'Nil') {
			return _elm_lang$core$Array$empty;
		} else {
			return A2(
				_elm_lang$core$Array$append,
				A2(_elm_lang$core$Array$push, _p0._0, _elm_lang$core$Array$empty),
				_elm_community$elm_lazy_list$Lazy_List$toArray(_p0._1));
		}
	};
	var _elm_community$elm_lazy_list$Lazy_List$toList = function (list) {
		var _p1 = _elm_lang$lazy$Lazy$force(list);
		if (_p1.ctor === 'Nil') {
			return _elm_lang$core$Native_List.fromArray(
				[]);
		} else {
			return A2(
				_elm_lang$core$List_ops['::'],
				_p1._0,
				_elm_community$elm_lazy_list$Lazy_List$toList(_p1._1));
		}
	};
	var _elm_community$elm_lazy_list$Lazy_List$foldr = F3(
		function (reducer, b, list) {
			return A3(
				_elm_lang$core$Array$foldr,
				reducer,
				b,
				_elm_community$elm_lazy_list$Lazy_List$toArray(list));
		});
	var _elm_community$elm_lazy_list$Lazy_List$reduce = F3(
		function (reducer, b, list) {
			reduce:
			while (true) {
				var _p2 = _elm_lang$lazy$Lazy$force(list);
				if (_p2.ctor === 'Nil') {
					return b;
				} else {
					var _v3 = reducer,
						_v4 = A2(reducer, _p2._0, b),
						_v5 = _p2._1;
					reducer = _v3;
					b = _v4;
					list = _v5;
					continue reduce;
				}
			}
		});
	var _elm_community$elm_lazy_list$Lazy_List$foldl = _elm_community$elm_lazy_list$Lazy_List$reduce;
	var _elm_community$elm_lazy_list$Lazy_List$sum = A2(
		_elm_community$elm_lazy_list$Lazy_List$reduce,
		F2(
			function (x, y) {
				return x + y;
			}),
		0);
	var _elm_community$elm_lazy_list$Lazy_List$product = A2(
		_elm_community$elm_lazy_list$Lazy_List$reduce,
		F2(
			function (x, y) {
				return x * y;
			}),
		1);
	var _elm_community$elm_lazy_list$Lazy_List$length = A2(
		_elm_community$elm_lazy_list$Lazy_List$reduce,
		F2(
			function (_p3, n) {
				return n + 1;
			}),
		0);
	var _elm_community$elm_lazy_list$Lazy_List$member = F2(
		function (a, list) {
			var _p4 = _elm_lang$lazy$Lazy$force(list);
			if (_p4.ctor === 'Nil') {
				return false;
			} else {
				return _elm_lang$core$Native_Utils.eq(_p4._0, a) || A2(_elm_community$elm_lazy_list$Lazy_List$member, a, _p4._1);
			}
		});
	var _elm_community$elm_lazy_list$Lazy_List$tail = function (list) {
		var _p5 = _elm_lang$lazy$Lazy$force(list);
		if (_p5.ctor === 'Nil') {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			return _elm_lang$core$Maybe$Just(_p5._1);
		}
	};
	var _elm_community$elm_lazy_list$Lazy_List$head = function (list) {
		var _p6 = _elm_lang$lazy$Lazy$force(list);
		if (_p6.ctor === 'Nil') {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			return _elm_lang$core$Maybe$Just(_p6._0);
		}
	};
	var _elm_community$elm_lazy_list$Lazy_List$isEmpty = function (list) {
		var _p7 = _elm_lang$lazy$Lazy$force(list);
		if (_p7.ctor === 'Nil') {
			return true;
		} else {
			return false;
		}
	};
	var _elm_community$elm_lazy_list$Lazy_List$Cons = F2(
		function (a, b) {
			return {ctor: 'Cons', _0: a, _1: b};
		});
	var _elm_community$elm_lazy_list$Lazy_List$cons = F2(
		function (a, list) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p8) {
					var _p9 = _p8;
					return A2(_elm_community$elm_lazy_list$Lazy_List$Cons, a, list);
				});
		});
	var _elm_community$elm_lazy_list$Lazy_List_ops = _elm_community$elm_lazy_list$Lazy_List_ops || {};
	_elm_community$elm_lazy_list$Lazy_List_ops[':::'] = _elm_community$elm_lazy_list$Lazy_List$cons;
	var _elm_community$elm_lazy_list$Lazy_List$append = F2(
		function (list1, list2) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p10) {
					var _p11 = _p10;
					var _p12 = _elm_lang$lazy$Lazy$force(list1);
					if (_p12.ctor === 'Nil') {
						return _elm_lang$lazy$Lazy$force(list2);
					} else {
						return _elm_lang$lazy$Lazy$force(
							A2(
								_elm_community$elm_lazy_list$Lazy_List_ops[':::'],
								_p12._0,
								A2(_elm_community$elm_lazy_list$Lazy_List_ops['+++'], _p12._1, list2)));
					}
				});
		});
	var _elm_community$elm_lazy_list$Lazy_List_ops = _elm_community$elm_lazy_list$Lazy_List_ops || {};
	_elm_community$elm_lazy_list$Lazy_List_ops['+++'] = _elm_community$elm_lazy_list$Lazy_List$append;
	var _elm_community$elm_lazy_list$Lazy_List$cycle = function (list) {
		return A2(
			_elm_community$elm_lazy_list$Lazy_List_ops['+++'],
			list,
			_elm_lang$lazy$Lazy$lazy(
				function (_p13) {
					var _p14 = _p13;
					return _elm_lang$lazy$Lazy$force(
						_elm_community$elm_lazy_list$Lazy_List$cycle(list));
				}));
	};
	var _elm_community$elm_lazy_list$Lazy_List$interleave = F2(
		function (list1, list2) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p15) {
					var _p16 = _p15;
					var _p17 = _elm_lang$lazy$Lazy$force(list1);
					if (_p17.ctor === 'Nil') {
						return _elm_lang$lazy$Lazy$force(list2);
					} else {
						var _p18 = _elm_lang$lazy$Lazy$force(list2);
						if (_p18.ctor === 'Nil') {
							return _elm_lang$lazy$Lazy$force(list1);
						} else {
							return _elm_lang$lazy$Lazy$force(
								A2(
									_elm_community$elm_lazy_list$Lazy_List_ops[':::'],
									_p17._0,
									A2(
										_elm_community$elm_lazy_list$Lazy_List_ops[':::'],
										_p18._0,
										A2(_elm_community$elm_lazy_list$Lazy_List$interleave, _p17._1, _p18._1))));
						}
					}
				});
		});
	var _elm_community$elm_lazy_list$Lazy_List$repeat = function (a) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p19) {
				var _p20 = _p19;
				return A2(
					_elm_community$elm_lazy_list$Lazy_List$Cons,
					a,
					_elm_community$elm_lazy_list$Lazy_List$repeat(a));
			});
	};
	var _elm_community$elm_lazy_list$Lazy_List$iterate = F2(
		function (f, a) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p21) {
					var _p22 = _p21;
					return A2(
						_elm_community$elm_lazy_list$Lazy_List$Cons,
						a,
						A2(
							_elm_community$elm_lazy_list$Lazy_List$iterate,
							f,
							f(a)));
				});
		});
	var _elm_community$elm_lazy_list$Lazy_List$numbers = A2(
		_elm_community$elm_lazy_list$Lazy_List$iterate,
		F2(
			function (x, y) {
				return x + y;
			})(1),
		1);
	var _elm_community$elm_lazy_list$Lazy_List$Nil = {ctor: 'Nil'};
	var _elm_community$elm_lazy_list$Lazy_List$empty = _elm_lang$lazy$Lazy$lazy(
		function (_p23) {
			var _p24 = _p23;
			return _elm_community$elm_lazy_list$Lazy_List$Nil;
		});
	var _elm_community$elm_lazy_list$Lazy_List$singleton = function (a) {
		return A2(_elm_community$elm_lazy_list$Lazy_List$cons, a, _elm_community$elm_lazy_list$Lazy_List$empty);
	};
	var _elm_community$elm_lazy_list$Lazy_List$reverse = A2(_elm_community$elm_lazy_list$Lazy_List$reduce, _elm_community$elm_lazy_list$Lazy_List$cons, _elm_community$elm_lazy_list$Lazy_List$empty);
	var _elm_community$elm_lazy_list$Lazy_List$fromList = A2(_elm_lang$core$List$foldr, _elm_community$elm_lazy_list$Lazy_List$cons, _elm_community$elm_lazy_list$Lazy_List$empty);
	var _elm_community$elm_lazy_list$Lazy_List$fromArray = A2(_elm_lang$core$Array$foldr, _elm_community$elm_lazy_list$Lazy_List$cons, _elm_community$elm_lazy_list$Lazy_List$empty);
	var _elm_community$elm_lazy_list$Lazy_List$intersperse = F2(
		function (a, list) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p25) {
					var _p26 = _p25;
					var _p27 = _elm_lang$lazy$Lazy$force(list);
					if (_p27.ctor === 'Nil') {
						return _elm_community$elm_lazy_list$Lazy_List$Nil;
					} else {
						var _p32 = _p27._0;
						var _p28 = _elm_lang$lazy$Lazy$force(_p27._1);
						if (_p28.ctor === 'Nil') {
							return _elm_lang$lazy$Lazy$force(
								A2(_elm_community$elm_lazy_list$Lazy_List_ops[':::'], _p32, _elm_community$elm_lazy_list$Lazy_List$empty));
						} else {
							var _p31 = _p28._1;
							var _p30 = _p28._0;
							var _p29 = _elm_lang$lazy$Lazy$force(_p31);
							if (_p29.ctor === 'Nil') {
								return _elm_lang$lazy$Lazy$force(
									A2(
										_elm_community$elm_lazy_list$Lazy_List_ops[':::'],
										_p32,
										A2(
											_elm_community$elm_lazy_list$Lazy_List_ops[':::'],
											a,
											A2(_elm_community$elm_lazy_list$Lazy_List_ops[':::'], _p30, _elm_community$elm_lazy_list$Lazy_List$empty))));
							} else {
								return _elm_lang$lazy$Lazy$force(
									A2(
										_elm_community$elm_lazy_list$Lazy_List_ops[':::'],
										_p32,
										A2(
											_elm_community$elm_lazy_list$Lazy_List_ops[':::'],
											a,
											A2(
												_elm_community$elm_lazy_list$Lazy_List_ops[':::'],
												_p30,
												A2(
													_elm_community$elm_lazy_list$Lazy_List_ops[':::'],
													a,
													A2(_elm_community$elm_lazy_list$Lazy_List$intersperse, a, _p31))))));
							}
						}
					}
				});
		});
	var _elm_community$elm_lazy_list$Lazy_List$take = F2(
		function (n, list) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p33) {
					var _p34 = _p33;
					if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
						return _elm_community$elm_lazy_list$Lazy_List$Nil;
					} else {
						var _p35 = _elm_lang$lazy$Lazy$force(list);
						if (_p35.ctor === 'Nil') {
							return _elm_community$elm_lazy_list$Lazy_List$Nil;
						} else {
							return A2(
								_elm_community$elm_lazy_list$Lazy_List$Cons,
								_p35._0,
								A2(_elm_community$elm_lazy_list$Lazy_List$take, n - 1, _p35._1));
						}
					}
				});
		});
	var _elm_community$elm_lazy_list$Lazy_List$takeWhile = F2(
		function (predicate, list) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p36) {
					var _p37 = _p36;
					var _p38 = _elm_lang$lazy$Lazy$force(list);
					if (_p38.ctor === 'Nil') {
						return _elm_community$elm_lazy_list$Lazy_List$Nil;
					} else {
						var _p39 = _p38._0;
						return predicate(_p39) ? A2(
							_elm_community$elm_lazy_list$Lazy_List$Cons,
							_p39,
							A2(_elm_community$elm_lazy_list$Lazy_List$takeWhile, predicate, _p38._1)) : _elm_community$elm_lazy_list$Lazy_List$Nil;
					}
				});
		});
	var _elm_community$elm_lazy_list$Lazy_List$drop = F2(
		function (n, list) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p40) {
					var _p41 = _p40;
					if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
						return _elm_lang$lazy$Lazy$force(list);
					} else {
						var _p42 = _elm_lang$lazy$Lazy$force(list);
						if (_p42.ctor === 'Nil') {
							return _elm_community$elm_lazy_list$Lazy_List$Nil;
						} else {
							return _elm_lang$lazy$Lazy$force(
								A2(_elm_community$elm_lazy_list$Lazy_List$drop, n - 1, _p42._1));
						}
					}
				});
		});
	var _elm_community$elm_lazy_list$Lazy_List$dropWhile = F2(
		function (predicate, list) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p43) {
					var _p44 = _p43;
					var _p45 = _elm_lang$lazy$Lazy$force(list);
					if (_p45.ctor === 'Nil') {
						return _elm_community$elm_lazy_list$Lazy_List$Nil;
					} else {
						return predicate(_p45._0) ? _elm_lang$lazy$Lazy$force(
							A2(_elm_community$elm_lazy_list$Lazy_List$dropWhile, predicate, _p45._1)) : _elm_lang$lazy$Lazy$force(list);
					}
				});
		});
	var _elm_community$elm_lazy_list$Lazy_List$unique = function (list) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p46) {
				var _p47 = _p46;
				var _p48 = _elm_lang$lazy$Lazy$force(list);
				if (_p48.ctor === 'Nil') {
					return _elm_community$elm_lazy_list$Lazy_List$Nil;
				} else {
					var _p50 = _p48._1;
					var _p49 = _p48._0;
					return A2(_elm_community$elm_lazy_list$Lazy_List$member, _p49, _p50) ? _elm_lang$lazy$Lazy$force(
						_elm_community$elm_lazy_list$Lazy_List$unique(_p50)) : A2(
						_elm_community$elm_lazy_list$Lazy_List$Cons,
						_p49,
						_elm_community$elm_lazy_list$Lazy_List$unique(_p50));
				}
			});
	};
	var _elm_community$elm_lazy_list$Lazy_List$keepIf = F2(
		function (predicate, list) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p51) {
					var _p52 = _p51;
					var _p53 = _elm_lang$lazy$Lazy$force(list);
					if (_p53.ctor === 'Nil') {
						return _elm_community$elm_lazy_list$Lazy_List$Nil;
					} else {
						var _p55 = _p53._1;
						var _p54 = _p53._0;
						return predicate(_p54) ? A2(
							_elm_community$elm_lazy_list$Lazy_List$Cons,
							_p54,
							A2(_elm_community$elm_lazy_list$Lazy_List$keepIf, predicate, _p55)) : _elm_lang$lazy$Lazy$force(
							A2(_elm_community$elm_lazy_list$Lazy_List$keepIf, predicate, _p55));
					}
				});
		});
	var _elm_community$elm_lazy_list$Lazy_List$dropIf = function (predicate) {
		return _elm_community$elm_lazy_list$Lazy_List$keepIf(
			function (n) {
				return _elm_lang$core$Basics$not(
					predicate(n));
			});
	};
	var _elm_community$elm_lazy_list$Lazy_List$flatten = function (list) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p56) {
				var _p57 = _p56;
				var _p58 = _elm_lang$lazy$Lazy$force(list);
				if (_p58.ctor === 'Nil') {
					return _elm_community$elm_lazy_list$Lazy_List$Nil;
				} else {
					return _elm_lang$lazy$Lazy$force(
						A2(
							_elm_community$elm_lazy_list$Lazy_List_ops['+++'],
							_p58._0,
							_elm_community$elm_lazy_list$Lazy_List$flatten(_p58._1)));
				}
			});
	};
	var _elm_community$elm_lazy_list$Lazy_List$map = F2(
		function (f, list) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p59) {
					var _p60 = _p59;
					var _p61 = _elm_lang$lazy$Lazy$force(list);
					if (_p61.ctor === 'Nil') {
						return _elm_community$elm_lazy_list$Lazy_List$Nil;
					} else {
						return A2(
							_elm_community$elm_lazy_list$Lazy_List$Cons,
							f(_p61._0),
							A2(_elm_community$elm_lazy_list$Lazy_List$map, f, _p61._1));
					}
				});
		});
	var _elm_community$elm_lazy_list$Lazy_List$flatMap = function (f) {
		return function (_p62) {
			return _elm_community$elm_lazy_list$Lazy_List$flatten(
				A2(_elm_community$elm_lazy_list$Lazy_List$map, f, _p62));
		};
	};
	var _elm_community$elm_lazy_list$Lazy_List$andThen = _elm_lang$core$Basics$flip(_elm_community$elm_lazy_list$Lazy_List$flatMap);
	var _elm_community$elm_lazy_list$Lazy_List$map2 = F3(
		function (f, list1, list2) {
			return _elm_lang$lazy$Lazy$lazy(
				function (_p63) {
					var _p64 = _p63;
					var _p65 = _elm_lang$lazy$Lazy$force(list1);
					if (_p65.ctor === 'Nil') {
						return _elm_community$elm_lazy_list$Lazy_List$Nil;
					} else {
						var _p66 = _elm_lang$lazy$Lazy$force(list2);
						if (_p66.ctor === 'Nil') {
							return _elm_community$elm_lazy_list$Lazy_List$Nil;
						} else {
							return A2(
								_elm_community$elm_lazy_list$Lazy_List$Cons,
								A2(f, _p65._0, _p66._0),
								A3(_elm_community$elm_lazy_list$Lazy_List$map2, f, _p65._1, _p66._1));
						}
					}
				});
		});
	var _elm_community$elm_lazy_list$Lazy_List$andMap = _elm_community$elm_lazy_list$Lazy_List$map2(
		F2(
			function (x, y) {
				return x(y);
			}));
	var _elm_community$elm_lazy_list$Lazy_List$map3 = F4(
		function (f, l1, l2, l3) {
			return A2(
				_elm_community$elm_lazy_list$Lazy_List$andMap,
				A2(
					_elm_community$elm_lazy_list$Lazy_List$andMap,
					A2(_elm_community$elm_lazy_list$Lazy_List$map, f, l1),
					l2),
				l3);
		});
	var _elm_community$elm_lazy_list$Lazy_List$zip3 = _elm_community$elm_lazy_list$Lazy_List$map3(
		F3(
			function (v0, v1, v2) {
				return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
			}));
	var _elm_community$elm_lazy_list$Lazy_List$map4 = F5(
		function (f, l1, l2, l3, l4) {
			return A2(
				_elm_community$elm_lazy_list$Lazy_List$andMap,
				A2(
					_elm_community$elm_lazy_list$Lazy_List$andMap,
					A2(
						_elm_community$elm_lazy_list$Lazy_List$andMap,
						A2(_elm_community$elm_lazy_list$Lazy_List$map, f, l1),
						l2),
					l3),
				l4);
		});
	var _elm_community$elm_lazy_list$Lazy_List$zip4 = _elm_community$elm_lazy_list$Lazy_List$map4(
		F4(
			function (v0, v1, v2, v3) {
				return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
			}));
	var _elm_community$elm_lazy_list$Lazy_List$map5 = F6(
		function (f, l1, l2, l3, l4, l5) {
			return A2(
				_elm_community$elm_lazy_list$Lazy_List$andMap,
				A2(
					_elm_community$elm_lazy_list$Lazy_List$andMap,
					A2(
						_elm_community$elm_lazy_list$Lazy_List$andMap,
						A2(
							_elm_community$elm_lazy_list$Lazy_List$andMap,
							A2(_elm_community$elm_lazy_list$Lazy_List$map, f, l1),
							l2),
						l3),
					l4),
				l5);
		});
	var _elm_community$elm_lazy_list$Lazy_List$zip5 = _elm_community$elm_lazy_list$Lazy_List$map5(
		F5(
			function (v0, v1, v2, v3, v4) {
				return {ctor: '_Tuple5', _0: v0, _1: v1, _2: v2, _3: v3, _4: v4};
			}));
	var _elm_community$elm_lazy_list$Lazy_List$zip = _elm_community$elm_lazy_list$Lazy_List$map2(
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}));
	
	var _elm_community$maybe_extra$Maybe_Extra$filter = F2(
		function (f, m) {
			var _p0 = A2(_elm_lang$core$Maybe$map, f, m);
			if ((_p0.ctor === 'Just') && (_p0._0 === true)) {
				return m;
			} else {
				return _elm_lang$core$Maybe$Nothing;
			}
		});
	var _elm_community$maybe_extra$Maybe_Extra$traverseArray = function (f) {
		var step = F2(
			function (e, acc) {
				var _p1 = f(e);
				if (_p1.ctor === 'Nothing') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					return A2(
						_elm_lang$core$Maybe$map,
						_elm_lang$core$Array$push(_p1._0),
						acc);
				}
			});
		return A2(
			_elm_lang$core$Array$foldl,
			step,
			_elm_lang$core$Maybe$Just(_elm_lang$core$Array$empty));
	};
	var _elm_community$maybe_extra$Maybe_Extra$combineArray = _elm_community$maybe_extra$Maybe_Extra$traverseArray(_elm_lang$core$Basics$identity);
	var _elm_community$maybe_extra$Maybe_Extra$traverse = function (f) {
		var step = F2(
			function (e, acc) {
				var _p2 = f(e);
				if (_p2.ctor === 'Nothing') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					return A2(
						_elm_lang$core$Maybe$map,
						F2(
							function (x, y) {
								return A2(_elm_lang$core$List_ops['::'], x, y);
							})(_p2._0),
						acc);
				}
			});
		return A2(
			_elm_lang$core$List$foldr,
			step,
			_elm_lang$core$Maybe$Just(
				_elm_lang$core$Native_List.fromArray(
					[])));
	};
	var _elm_community$maybe_extra$Maybe_Extra$combine = _elm_community$maybe_extra$Maybe_Extra$traverse(_elm_lang$core$Basics$identity);
	var _elm_community$maybe_extra$Maybe_Extra$maybeToArray = function (m) {
		var _p3 = m;
		if (_p3.ctor === 'Nothing') {
			return _elm_lang$core$Array$empty;
		} else {
			return A2(_elm_lang$core$Array$repeat, 1, _p3._0);
		}
	};
	var _elm_community$maybe_extra$Maybe_Extra$maybeToList = function (m) {
		var _p4 = m;
		if (_p4.ctor === 'Nothing') {
			return _elm_lang$core$Native_List.fromArray(
				[]);
		} else {
			return _elm_lang$core$Native_List.fromArray(
				[_p4._0]);
		}
	};
	var _elm_community$maybe_extra$Maybe_Extra$or = F2(
		function (ma, mb) {
			var _p5 = ma;
			if (_p5.ctor === 'Nothing') {
				return mb;
			} else {
				return ma;
			}
		});
	var _elm_community$maybe_extra$Maybe_Extra$prev = _elm_lang$core$Maybe$map2(_elm_lang$core$Basics$always);
	var _elm_community$maybe_extra$Maybe_Extra$next = _elm_lang$core$Maybe$map2(
		_elm_lang$core$Basics$flip(_elm_lang$core$Basics$always));
	var _elm_community$maybe_extra$Maybe_Extra$andMap = F2(
		function (f, x) {
			return A2(
				_elm_lang$core$Maybe$andThen,
				x,
				function (x$) {
					return A2(
						_elm_lang$core$Maybe$andThen,
						f,
						function (f$) {
							return _elm_lang$core$Maybe$Just(
								f$(x$));
						});
				});
		});
	var _elm_community$maybe_extra$Maybe_Extra$mapDefault = F3(
		function (d, f, m) {
			var _p6 = m;
			if (_p6.ctor === 'Nothing') {
				return d;
			} else {
				return f(_p6._0);
			}
		});
	var _elm_community$maybe_extra$Maybe_Extra$isJust = function (m) {
		var _p7 = m;
		if (_p7.ctor === 'Nothing') {
			return false;
		} else {
			return true;
		}
	};
	var _elm_community$maybe_extra$Maybe_Extra$isNothing = function (m) {
		var _p8 = m;
		if (_p8.ctor === 'Nothing') {
			return true;
		} else {
			return false;
		}
	};
	var _elm_community$maybe_extra$Maybe_Extra$join = function (mx) {
		var _p9 = mx;
		if (_p9.ctor === 'Just') {
			return _p9._0;
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	};
	var _elm_community$maybe_extra$Maybe_Extra_ops = _elm_community$maybe_extra$Maybe_Extra_ops || {};
	_elm_community$maybe_extra$Maybe_Extra_ops['?'] = F2(
		function (mx, x) {
			return A2(_elm_lang$core$Maybe$withDefault, x, mx);
		});
	
	//import Maybe, Native.Array, Native.List, Native.Utils, Result //
	
	var _elm_lang$core$Native_Json = function() {
	
	
	// CORE DECODERS
	
	function succeed(msg)
	{
		return {
			ctor: '<decoder>',
			tag: 'succeed',
			msg: msg
		};
	}
	
	function fail(msg)
	{
		return {
			ctor: '<decoder>',
			tag: 'fail',
			msg: msg
		};
	}
	
	function decodePrimitive(tag)
	{
		return {
			ctor: '<decoder>',
			tag: tag
		};
	}
	
	function decodeContainer(tag, decoder)
	{
		return {
			ctor: '<decoder>',
			tag: tag,
			decoder: decoder
		};
	}
	
	function decodeNull(value)
	{
		return {
			ctor: '<decoder>',
			tag: 'null',
			value: value
		};
	}
	
	function decodeField(field, decoder)
	{
		return {
			ctor: '<decoder>',
			tag: 'field',
			field: field,
			decoder: decoder
		};
	}
	
	function decodeKeyValuePairs(decoder)
	{
		return {
			ctor: '<decoder>',
			tag: 'key-value',
			decoder: decoder
		};
	}
	
	function decodeObject(f, decoders)
	{
		return {
			ctor: '<decoder>',
			tag: 'map-many',
			func: f,
			decoders: decoders
		};
	}
	
	function decodeTuple(f, decoders)
	{
		return {
			ctor: '<decoder>',
			tag: 'tuple',
			func: f,
			decoders: decoders
		};
	}
	
	function andThen(decoder, callback)
	{
		return {
			ctor: '<decoder>',
			tag: 'andThen',
			decoder: decoder,
			callback: callback
		};
	}
	
	function customAndThen(decoder, callback)
	{
		return {
			ctor: '<decoder>',
			tag: 'customAndThen',
			decoder: decoder,
			callback: callback
		};
	}
	
	function oneOf(decoders)
	{
		return {
			ctor: '<decoder>',
			tag: 'oneOf',
			decoders: decoders
		};
	}
	
	
	// DECODING OBJECTS
	
	function decodeObject1(f, d1)
	{
		return decodeObject(f, [d1]);
	}
	
	function decodeObject2(f, d1, d2)
	{
		return decodeObject(f, [d1, d2]);
	}
	
	function decodeObject3(f, d1, d2, d3)
	{
		return decodeObject(f, [d1, d2, d3]);
	}
	
	function decodeObject4(f, d1, d2, d3, d4)
	{
		return decodeObject(f, [d1, d2, d3, d4]);
	}
	
	function decodeObject5(f, d1, d2, d3, d4, d5)
	{
		return decodeObject(f, [d1, d2, d3, d4, d5]);
	}
	
	function decodeObject6(f, d1, d2, d3, d4, d5, d6)
	{
		return decodeObject(f, [d1, d2, d3, d4, d5, d6]);
	}
	
	function decodeObject7(f, d1, d2, d3, d4, d5, d6, d7)
	{
		return decodeObject(f, [d1, d2, d3, d4, d5, d6, d7]);
	}
	
	function decodeObject8(f, d1, d2, d3, d4, d5, d6, d7, d8)
	{
		return decodeObject(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
	}
	
	
	// DECODING TUPLES
	
	function decodeTuple1(f, d1)
	{
		return decodeTuple(f, [d1]);
	}
	
	function decodeTuple2(f, d1, d2)
	{
		return decodeTuple(f, [d1, d2]);
	}
	
	function decodeTuple3(f, d1, d2, d3)
	{
		return decodeTuple(f, [d1, d2, d3]);
	}
	
	function decodeTuple4(f, d1, d2, d3, d4)
	{
		return decodeTuple(f, [d1, d2, d3, d4]);
	}
	
	function decodeTuple5(f, d1, d2, d3, d4, d5)
	{
		return decodeTuple(f, [d1, d2, d3, d4, d5]);
	}
	
	function decodeTuple6(f, d1, d2, d3, d4, d5, d6)
	{
		return decodeTuple(f, [d1, d2, d3, d4, d5, d6]);
	}
	
	function decodeTuple7(f, d1, d2, d3, d4, d5, d6, d7)
	{
		return decodeTuple(f, [d1, d2, d3, d4, d5, d6, d7]);
	}
	
	function decodeTuple8(f, d1, d2, d3, d4, d5, d6, d7, d8)
	{
		return decodeTuple(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
	}
	
	
	// DECODE HELPERS
	
	function ok(value)
	{
		return { tag: 'ok', value: value };
	}
	
	function badPrimitive(type, value)
	{
		return { tag: 'primitive', type: type, value: value };
	}
	
	function badIndex(index, nestedProblems)
	{
		return { tag: 'index', index: index, rest: nestedProblems };
	}
	
	function badField(field, nestedProblems)
	{
		return { tag: 'field', field: field, rest: nestedProblems };
	}
	
	function badOneOf(problems)
	{
		return { tag: 'oneOf', problems: problems };
	}
	
	function bad(msg)
	{
		return { tag: 'fail', msg: msg };
	}
	
	function badToString(problem)
	{
		var context = '_';
		while (problem)
		{
			switch (problem.tag)
			{
				case 'primitive':
					return 'Expecting ' + problem.type
						+ (context === '_' ? '' : ' at ' + context)
						+ ' but instead got: ' + jsToString(problem.value);
	
				case 'index':
					context += '[' + problem.index + ']';
					problem = problem.rest;
					break;
	
				case 'field':
					context += '.' + problem.field;
					problem = problem.rest;
					break;
	
				case 'oneOf':
					var problems = problem.problems;
					for (var i = 0; i < problems.length; i++)
					{
						problems[i] = badToString(problems[i]);
					}
					return 'I ran into the following problems'
						+ (context === '_' ? '' : ' at ' + context)
						+ ':\n\n' + problems.join('\n');
	
				case 'fail':
					return 'I ran into a `fail` decoder'
						+ (context === '_' ? '' : ' at ' + context)
						+ ': ' + problem.msg;
			}
		}
	}
	
	function jsToString(value)
	{
		return value === undefined
			? 'undefined'
			: JSON.stringify(value);
	}
	
	
	// DECODE
	
	function runOnString(decoder, string)
	{
		var json;
		try
		{
			json = JSON.parse(string);
		}
		catch (e)
		{
			return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
		}
		return run(decoder, json);
	}
	
	function run(decoder, value)
	{
		var result = runHelp(decoder, value);
		return (result.tag === 'ok')
			? _elm_lang$core$Result$Ok(result.value)
			: _elm_lang$core$Result$Err(badToString(result));
	}
	
	function runHelp(decoder, value)
	{
		switch (decoder.tag)
		{
			case 'bool':
				return (typeof value === 'boolean')
					? ok(value)
					: badPrimitive('a Bool', value);
	
			case 'int':
				if (typeof value !== 'number') {
					return badPrimitive('an Int', value);
				}
	
				if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
					return ok(value);
				}
	
				if (isFinite(value) && !(value % 1)) {
					return ok(value);
				}
	
				return badPrimitive('an Int', value);
	
			case 'float':
				return (typeof value === 'number')
					? ok(value)
					: badPrimitive('a Float', value);
	
			case 'string':
				return (typeof value === 'string')
					? ok(value)
					: (value instanceof String)
						? ok(value + '')
						: badPrimitive('a String', value);
	
			case 'null':
				return (value === null)
					? ok(decoder.value)
					: badPrimitive('null', value);
	
			case 'value':
				return ok(value);
	
			case 'list':
				if (!(value instanceof Array))
				{
					return badPrimitive('a List', value);
				}
	
				var list = _elm_lang$core$Native_List.Nil;
				for (var i = value.length; i--; )
				{
					var result = runHelp(decoder.decoder, value[i]);
					if (result.tag !== 'ok')
					{
						return badIndex(i, result)
					}
					list = _elm_lang$core$Native_List.Cons(result.value, list);
				}
				return ok(list);
	
			case 'array':
				if (!(value instanceof Array))
				{
					return badPrimitive('an Array', value);
				}
	
				var len = value.length;
				var array = new Array(len);
				for (var i = len; i--; )
				{
					var result = runHelp(decoder.decoder, value[i]);
					if (result.tag !== 'ok')
					{
						return badIndex(i, result);
					}
					array[i] = result.value;
				}
				return ok(_elm_lang$core$Native_Array.fromJSArray(array));
	
			case 'maybe':
				var result = runHelp(decoder.decoder, value);
				return (result.tag === 'ok')
					? ok(_elm_lang$core$Maybe$Just(result.value))
					: ok(_elm_lang$core$Maybe$Nothing);
	
			case 'field':
				var field = decoder.field;
				if (typeof value !== 'object' || value === null || !(field in value))
				{
					return badPrimitive('an object with a field named `' + field + '`', value);
				}
	
				var result = runHelp(decoder.decoder, value[field]);
				return (result.tag === 'ok')
					? result
					: badField(field, result);
	
			case 'key-value':
				if (typeof value !== 'object' || value === null || value instanceof Array)
				{
					return badPrimitive('an object', value);
				}
	
				var keyValuePairs = _elm_lang$core$Native_List.Nil;
				for (var key in value)
				{
					var result = runHelp(decoder.decoder, value[key]);
					if (result.tag !== 'ok')
					{
						return badField(key, result);
					}
					var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
					keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
				}
				return ok(keyValuePairs);
	
			case 'map-many':
				var answer = decoder.func;
				var decoders = decoder.decoders;
				for (var i = 0; i < decoders.length; i++)
				{
					var result = runHelp(decoders[i], value);
					if (result.tag !== 'ok')
					{
						return result;
					}
					answer = answer(result.value);
				}
				return ok(answer);
	
			case 'tuple':
				var decoders = decoder.decoders;
				var len = decoders.length;
	
				if ( !(value instanceof Array) || value.length !== len )
				{
					return badPrimitive('a Tuple with ' + len + ' entries', value);
				}
	
				var answer = decoder.func;
				for (var i = 0; i < len; i++)
				{
					var result = runHelp(decoders[i], value[i]);
					if (result.tag !== 'ok')
					{
						return badIndex(i, result);
					}
					answer = answer(result.value);
				}
				return ok(answer);
	
			case 'customAndThen':
				var result = runHelp(decoder.decoder, value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				var realResult = decoder.callback(result.value);
				if (realResult.ctor === 'Err')
				{
					return badPrimitive('something custom', value);
				}
				return ok(realResult._0);
	
			case 'andThen':
				var result = runHelp(decoder.decoder, value);
				return (result.tag !== 'ok')
					? result
					: runHelp(decoder.callback(result.value), value);
	
			case 'oneOf':
				var errors = [];
				var temp = decoder.decoders;
				while (temp.ctor !== '[]')
				{
					var result = runHelp(temp._0, value);
	
					if (result.tag === 'ok')
					{
						return result;
					}
	
					errors.push(result);
	
					temp = temp._1;
				}
				return badOneOf(errors);
	
			case 'fail':
				return bad(decoder.msg);
	
			case 'succeed':
				return ok(decoder.msg);
		}
	}
	
	
	// EQUALITY
	
	function equality(a, b)
	{
		if (a === b)
		{
			return true;
		}
	
		if (a.tag !== b.tag)
		{
			return false;
		}
	
		switch (a.tag)
		{
			case 'succeed':
			case 'fail':
				return a.msg === b.msg;
	
			case 'bool':
			case 'int':
			case 'float':
			case 'string':
			case 'value':
				return true;
	
			case 'null':
				return a.value === b.value;
	
			case 'list':
			case 'array':
			case 'maybe':
			case 'key-value':
				return equality(a.decoder, b.decoder);
	
			case 'field':
				return a.field === b.field && equality(a.decoder, b.decoder);
	
			case 'map-many':
			case 'tuple':
				if (a.func !== b.func)
				{
					return false;
				}
				return listEquality(a.decoders, b.decoders);
	
			case 'andThen':
			case 'customAndThen':
				return a.callback === b.callback && equality(a.decoder, b.decoder);
	
			case 'oneOf':
				return listEquality(a.decoders, b.decoders);
		}
	}
	
	function listEquality(aDecoders, bDecoders)
	{
		var len = aDecoders.length;
		if (len !== bDecoders.length)
		{
			return false;
		}
		for (var i = 0; i < len; i++)
		{
			if (!equality(aDecoders[i], bDecoders[i]))
			{
				return false;
			}
		}
		return true;
	}
	
	
	// ENCODE
	
	function encode(indentLevel, value)
	{
		return JSON.stringify(value, null, indentLevel);
	}
	
	function identity(value)
	{
		return value;
	}
	
	function encodeObject(keyValuePairs)
	{
		var obj = {};
		while (keyValuePairs.ctor !== '[]')
		{
			var pair = keyValuePairs._0;
			obj[pair._0] = pair._1;
			keyValuePairs = keyValuePairs._1;
		}
		return obj;
	}
	
	return {
		encode: F2(encode),
		runOnString: F2(runOnString),
		run: F2(run),
	
		decodeNull: decodeNull,
		decodePrimitive: decodePrimitive,
		decodeContainer: F2(decodeContainer),
	
		decodeField: F2(decodeField),
	
		decodeObject1: F2(decodeObject1),
		decodeObject2: F3(decodeObject2),
		decodeObject3: F4(decodeObject3),
		decodeObject4: F5(decodeObject4),
		decodeObject5: F6(decodeObject5),
		decodeObject6: F7(decodeObject6),
		decodeObject7: F8(decodeObject7),
		decodeObject8: F9(decodeObject8),
		decodeKeyValuePairs: decodeKeyValuePairs,
	
		decodeTuple1: F2(decodeTuple1),
		decodeTuple2: F3(decodeTuple2),
		decodeTuple3: F4(decodeTuple3),
		decodeTuple4: F5(decodeTuple4),
		decodeTuple5: F6(decodeTuple5),
		decodeTuple6: F7(decodeTuple6),
		decodeTuple7: F8(decodeTuple7),
		decodeTuple8: F9(decodeTuple8),
	
		andThen: F2(andThen),
		customAndThen: F2(customAndThen),
		fail: fail,
		succeed: succeed,
		oneOf: oneOf,
	
		identity: identity,
		encodeNull: null,
		encodeArray: _elm_lang$core$Native_Array.toJSArray,
		encodeList: _elm_lang$core$Native_List.toArray,
		encodeObject: encodeObject,
	
		equality: equality
	};
	
	}();
	
	var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
	var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
	var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
	var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
	var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
	var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
	var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
	var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
	var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
	var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};
	
	var _elm_lang$core$Json_Decode$tuple8 = _elm_lang$core$Native_Json.decodeTuple8;
	var _elm_lang$core$Json_Decode$tuple7 = _elm_lang$core$Native_Json.decodeTuple7;
	var _elm_lang$core$Json_Decode$tuple6 = _elm_lang$core$Native_Json.decodeTuple6;
	var _elm_lang$core$Json_Decode$tuple5 = _elm_lang$core$Native_Json.decodeTuple5;
	var _elm_lang$core$Json_Decode$tuple4 = _elm_lang$core$Native_Json.decodeTuple4;
	var _elm_lang$core$Json_Decode$tuple3 = _elm_lang$core$Native_Json.decodeTuple3;
	var _elm_lang$core$Json_Decode$tuple2 = _elm_lang$core$Native_Json.decodeTuple2;
	var _elm_lang$core$Json_Decode$tuple1 = _elm_lang$core$Native_Json.decodeTuple1;
	var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
	var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
	var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
	var _elm_lang$core$Json_Decode$customDecoder = _elm_lang$core$Native_Json.customAndThen;
	var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
	var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
	var _elm_lang$core$Json_Decode$maybe = function (decoder) {
		return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
	};
	var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
	var _elm_lang$core$Json_Decode$array = function (decoder) {
		return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
	};
	var _elm_lang$core$Json_Decode$list = function (decoder) {
		return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
	};
	var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
	var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
	var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
	var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
	var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
	var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
	var _elm_lang$core$Json_Decode$object8 = _elm_lang$core$Native_Json.decodeObject8;
	var _elm_lang$core$Json_Decode$object7 = _elm_lang$core$Native_Json.decodeObject7;
	var _elm_lang$core$Json_Decode$object6 = _elm_lang$core$Native_Json.decodeObject6;
	var _elm_lang$core$Json_Decode$object5 = _elm_lang$core$Native_Json.decodeObject5;
	var _elm_lang$core$Json_Decode$object4 = _elm_lang$core$Native_Json.decodeObject4;
	var _elm_lang$core$Json_Decode$object3 = _elm_lang$core$Native_Json.decodeObject3;
	var _elm_lang$core$Json_Decode$object2 = _elm_lang$core$Native_Json.decodeObject2;
	var _elm_lang$core$Json_Decode$object1 = _elm_lang$core$Native_Json.decodeObject1;
	var _elm_lang$core$Json_Decode_ops = _elm_lang$core$Json_Decode_ops || {};
	_elm_lang$core$Json_Decode_ops[':='] = _elm_lang$core$Native_Json.decodeField;
	var _elm_lang$core$Json_Decode$at = F2(
		function (fields, decoder) {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return A2(_elm_lang$core$Json_Decode_ops[':='], x, y);
					}),
				decoder,
				fields);
		});
	var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
	var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.decodeObject1;
	var _elm_lang$core$Json_Decode$dict = function (decoder) {
		return A2(
			_elm_lang$core$Json_Decode$map,
			_elm_lang$core$Dict$fromList,
			_elm_lang$core$Json_Decode$keyValuePairs(decoder));
	};
	var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};
	
	var _elm_lang$core$Process$kill = _elm_lang$core$Native_Scheduler.kill;
	var _elm_lang$core$Process$sleep = _elm_lang$core$Native_Scheduler.sleep;
	var _elm_lang$core$Process$spawn = _elm_lang$core$Native_Scheduler.spawn;
	
	var _elm_lang$dom$Native_Dom = function() {
	
	function on(node)
	{
		return function(eventName, decoder, toTask)
		{
			return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
	
				function performTask(event)
				{
					var result = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, event);
					if (result.ctor === 'Ok')
					{
						_elm_lang$core$Native_Scheduler.rawSpawn(toTask(result._0));
					}
				}
	
				node.addEventListener(eventName, performTask);
	
				return function()
				{
					node.removeEventListener(eventName, performTask);
				};
			});
		};
	}
	
	return {
		onDocument: F3(on(document)),
		onWindow: F3(on(window))
	};
	
	}();
	
	var _elm_lang$dom$Dom_LowLevel$onWindow = _elm_lang$dom$Native_Dom.onWindow;
	var _elm_lang$dom$Dom_LowLevel$onDocument = _elm_lang$dom$Native_Dom.onDocument;
	
	//import Native.Json //
	
	var _elm_lang$virtual_dom$Native_VirtualDom = function() {
	
	var STYLE_KEY = 'STYLE';
	var EVENT_KEY = 'EVENT';
	var ATTR_KEY = 'ATTR';
	var ATTR_NS_KEY = 'ATTR_NS';
	
	
	
	////////////  VIRTUAL DOM NODES  ////////////
	
	
	function text(string)
	{
		return {
			type: 'text',
			text: string
		};
	}
	
	
	function node(tag)
	{
		return F2(function(factList, kidList) {
			return nodeHelp(tag, factList, kidList);
		});
	}
	
	
	function nodeHelp(tag, factList, kidList)
	{
		var organized = organizeFacts(factList);
		var namespace = organized.namespace;
		var facts = organized.facts;
	
		var children = [];
		var descendantsCount = 0;
		while (kidList.ctor !== '[]')
		{
			var kid = kidList._0;
			descendantsCount += (kid.descendantsCount || 0);
			children.push(kid);
			kidList = kidList._1;
		}
		descendantsCount += children.length;
	
		return {
			type: 'node',
			tag: tag,
			facts: facts,
			children: children,
			namespace: namespace,
			descendantsCount: descendantsCount
		};
	}
	
	
	function keyedNode(tag, factList, kidList)
	{
		var organized = organizeFacts(factList);
		var namespace = organized.namespace;
		var facts = organized.facts;
	
		var children = [];
		var descendantsCount = 0;
		while (kidList.ctor !== '[]')
		{
			var kid = kidList._0;
			descendantsCount += (kid._1.descendantsCount || 0);
			children.push(kid);
			kidList = kidList._1;
		}
		descendantsCount += children.length;
	
		return {
			type: 'keyed-node',
			tag: tag,
			facts: facts,
			children: children,
			namespace: namespace,
			descendantsCount: descendantsCount
		};
	}
	
	
	function custom(factList, model, impl)
	{
		var facts = organizeFacts(factList).facts;
	
		return {
			type: 'custom',
			facts: facts,
			model: model,
			impl: impl
		};
	}
	
	
	function map(tagger, node)
	{
		return {
			type: 'tagger',
			tagger: tagger,
			node: node,
			descendantsCount: 1 + (node.descendantsCount || 0)
		};
	}
	
	
	function thunk(func, args, thunk)
	{
		return {
			type: 'thunk',
			func: func,
			args: args,
			thunk: thunk,
			node: undefined
		};
	}
	
	function lazy(fn, a)
	{
		return thunk(fn, [a], function() {
			return fn(a);
		});
	}
	
	function lazy2(fn, a, b)
	{
		return thunk(fn, [a,b], function() {
			return A2(fn, a, b);
		});
	}
	
	function lazy3(fn, a, b, c)
	{
		return thunk(fn, [a,b,c], function() {
			return A3(fn, a, b, c);
		});
	}
	
	
	
	// FACTS
	
	
	function organizeFacts(factList)
	{
		var namespace, facts = {};
	
		while (factList.ctor !== '[]')
		{
			var entry = factList._0;
			var key = entry.key;
	
			if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
			{
				var subFacts = facts[key] || {};
				subFacts[entry.realKey] = entry.value;
				facts[key] = subFacts;
			}
			else if (key === STYLE_KEY)
			{
				var styles = facts[key] || {};
				var styleList = entry.value;
				while (styleList.ctor !== '[]')
				{
					var style = styleList._0;
					styles[style._0] = style._1;
					styleList = styleList._1;
				}
				facts[key] = styles;
			}
			else if (key === 'namespace')
			{
				namespace = entry.value;
			}
			else
			{
				facts[key] = entry.value;
			}
			factList = factList._1;
		}
	
		return {
			facts: facts,
			namespace: namespace
		};
	}
	
	
	
	////////////  PROPERTIES AND ATTRIBUTES  ////////////
	
	
	function style(value)
	{
		return {
			key: STYLE_KEY,
			value: value
		};
	}
	
	
	function property(key, value)
	{
		return {
			key: key,
			value: value
		};
	}
	
	
	function attribute(key, value)
	{
		return {
			key: ATTR_KEY,
			realKey: key,
			value: value
		};
	}
	
	
	function attributeNS(namespace, key, value)
	{
		return {
			key: ATTR_NS_KEY,
			realKey: key,
			value: {
				value: value,
				namespace: namespace
			}
		};
	}
	
	
	function on(name, options, decoder)
	{
		return {
			key: EVENT_KEY,
			realKey: name,
			value: {
				options: options,
				decoder: decoder
			}
		};
	}
	
	
	function equalEvents(a, b)
	{
		if (!a.options === b.options)
		{
			if (a.stopPropagation !== b.stopPropagation || a.preventDefault !== b.preventDefault)
			{
				return false;
			}
		}
		return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
	}
	
	
	
	////////////  RENDERER  ////////////
	
	
	function renderer(parent, tagger, initialVirtualNode)
	{
		var eventNode = { tagger: tagger, parent: undefined };
	
		var domNode = render(initialVirtualNode, eventNode);
		parent.appendChild(domNode);
	
		var state = 'NO_REQUEST';
		var currentVirtualNode = initialVirtualNode;
		var nextVirtualNode = initialVirtualNode;
	
		function registerVirtualNode(vNode)
		{
			if (state === 'NO_REQUEST')
			{
				rAF(updateIfNeeded);
			}
			state = 'PENDING_REQUEST';
			nextVirtualNode = vNode;
		}
	
		function updateIfNeeded()
		{
			switch (state)
			{
				case 'NO_REQUEST':
					throw new Error(
						'Unexpected draw callback.\n' +
						'Please report this to <https://github.com/elm-lang/core/issues>.'
					);
	
				case 'PENDING_REQUEST':
					rAF(updateIfNeeded);
					state = 'EXTRA_REQUEST';
	
					var patches = diff(currentVirtualNode, nextVirtualNode);
					domNode = applyPatches(domNode, currentVirtualNode, patches, eventNode);
					currentVirtualNode = nextVirtualNode;
	
					return;
	
				case 'EXTRA_REQUEST':
					state = 'NO_REQUEST';
					return;
			}
		}
	
		return { update: registerVirtualNode };
	}
	
	
	var rAF =
		typeof requestAnimationFrame !== 'undefined'
			? requestAnimationFrame
			: function(cb) { setTimeout(cb, 1000 / 60); };
	
	
	
	////////////  RENDER  ////////////
	
	
	function render(vNode, eventNode)
	{
		switch (vNode.type)
		{
			case 'thunk':
				if (!vNode.node)
				{
					vNode.node = vNode.thunk();
				}
				return render(vNode.node, eventNode);
	
			case 'tagger':
				var subNode = vNode.node;
				var tagger = vNode.tagger;
	
				while (subNode.type === 'tagger')
				{
					typeof tagger !== 'object'
						? tagger = [tagger, subNode.tagger]
						: tagger.push(subNode.tagger);
	
					subNode = subNode.node;
				}
	
				var subEventRoot = {
					tagger: tagger,
					parent: eventNode
				};
	
				var domNode = render(subNode, subEventRoot);
				domNode.elm_event_node_ref = subEventRoot;
				return domNode;
	
			case 'text':
				return document.createTextNode(vNode.text);
	
			case 'node':
				var domNode = vNode.namespace
					? document.createElementNS(vNode.namespace, vNode.tag)
					: document.createElement(vNode.tag);
	
				applyFacts(domNode, eventNode, vNode.facts);
	
				var children = vNode.children;
	
				for (var i = 0; i < children.length; i++)
				{
					domNode.appendChild(render(children[i], eventNode));
				}
	
				return domNode;
	
			case 'keyed-node':
				var domNode = vNode.namespace
					? document.createElementNS(vNode.namespace, vNode.tag)
					: document.createElement(vNode.tag);
	
				applyFacts(domNode, eventNode, vNode.facts);
	
				var children = vNode.children;
	
				for (var i = 0; i < children.length; i++)
				{
					domNode.appendChild(render(children[i]._1, eventNode));
				}
	
				return domNode;
	
			case 'custom':
				var domNode = vNode.impl.render(vNode.model);
				applyFacts(domNode, eventNode, vNode.facts);
				return domNode;
		}
	}
	
	
	
	////////////  APPLY FACTS  ////////////
	
	
	function applyFacts(domNode, eventNode, facts)
	{
		for (var key in facts)
		{
			var value = facts[key];
	
			switch (key)
			{
				case STYLE_KEY:
					applyStyles(domNode, value);
					break;
	
				case EVENT_KEY:
					applyEvents(domNode, eventNode, value);
					break;
	
				case ATTR_KEY:
					applyAttrs(domNode, value);
					break;
	
				case ATTR_NS_KEY:
					applyAttrsNS(domNode, value);
					break;
	
				case 'value':
					if (domNode[key] !== value)
					{
						domNode[key] = value;
					}
					break;
	
				default:
					domNode[key] = value;
					break;
			}
		}
	}
	
	function applyStyles(domNode, styles)
	{
		var domNodeStyle = domNode.style;
	
		for (var key in styles)
		{
			domNodeStyle[key] = styles[key];
		}
	}
	
	function applyEvents(domNode, eventNode, events)
	{
		var allHandlers = domNode.elm_handlers || {};
	
		for (var key in events)
		{
			var handler = allHandlers[key];
			var value = events[key];
	
			if (typeof value === 'undefined')
			{
				domNode.removeEventListener(key, handler);
				allHandlers[key] = undefined;
			}
			else if (typeof handler === 'undefined')
			{
				var handler = makeEventHandler(eventNode, value);
				domNode.addEventListener(key, handler);
				allHandlers[key] = handler;
			}
			else
			{
				handler.info = value;
			}
		}
	
		domNode.elm_handlers = allHandlers;
	}
	
	function makeEventHandler(eventNode, info)
	{
		function eventHandler(event)
		{
			var info = eventHandler.info;
	
			var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);
	
			if (value.ctor === 'Ok')
			{
				var options = info.options;
				if (options.stopPropagation)
				{
					event.stopPropagation();
				}
				if (options.preventDefault)
				{
					event.preventDefault();
				}
	
				var message = value._0;
	
				var currentEventNode = eventNode;
				while (currentEventNode)
				{
					var tagger = currentEventNode.tagger;
					if (typeof tagger === 'function')
					{
						message = tagger(message);
					}
					else
					{
						for (var i = tagger.length; i--; )
						{
							message = tagger[i](message);
						}
					}
					currentEventNode = currentEventNode.parent;
				}
			}
		};
	
		eventHandler.info = info;
	
		return eventHandler;
	}
	
	function applyAttrs(domNode, attrs)
	{
		for (var key in attrs)
		{
			var value = attrs[key];
			if (typeof value === 'undefined')
			{
				domNode.removeAttribute(key);
			}
			else
			{
				domNode.setAttribute(key, value);
			}
		}
	}
	
	function applyAttrsNS(domNode, nsAttrs)
	{
		for (var key in nsAttrs)
		{
			var pair = nsAttrs[key];
			var namespace = pair.namespace;
			var value = pair.value;
	
			if (typeof value === 'undefined')
			{
				domNode.removeAttributeNS(namespace, key);
			}
			else
			{
				domNode.setAttributeNS(namespace, key, value);
			}
		}
	}
	
	
	
	////////////  DIFF  ////////////
	
	
	function diff(a, b)
	{
		var patches = [];
		diffHelp(a, b, patches, 0);
		return patches;
	}
	
	
	function makePatch(type, index, data)
	{
		return {
			index: index,
			type: type,
			data: data,
			domNode: undefined,
			eventNode: undefined
		};
	}
	
	
	function diffHelp(a, b, patches, index)
	{
		if (a === b)
		{
			return;
		}
	
		var aType = a.type;
		var bType = b.type;
	
		// Bail if you run into different types of nodes. Implies that the
		// structure has changed significantly and it's not worth a diff.
		if (aType !== bType)
		{
			patches.push(makePatch('p-redraw', index, b));
			return;
		}
	
		// Now we know that both nodes are the same type.
		switch (bType)
		{
			case 'thunk':
				var aArgs = a.args;
				var bArgs = b.args;
				var i = aArgs.length;
				var same = a.func === b.func && i === bArgs.length;
				while (same && i--)
				{
					same = aArgs[i] === bArgs[i];
				}
				if (same)
				{
					b.node = a.node;
					return;
				}
				b.node = b.thunk();
				var subPatches = [];
				diffHelp(a.node, b.node, subPatches, 0);
				if (subPatches.length > 0)
				{
					patches.push(makePatch('p-thunk', index, subPatches));
				}
				return;
	
			case 'tagger':
				// gather nested taggers
				var aTaggers = a.tagger;
				var bTaggers = b.tagger;
				var nesting = false;
	
				var aSubNode = a.node;
				while (aSubNode.type === 'tagger')
				{
					nesting = true;
	
					typeof aTaggers !== 'object'
						? aTaggers = [aTaggers, aSubNode.tagger]
						: aTaggers.push(aSubNode.tagger);
	
					aSubNode = aSubNode.node;
				}
	
				var bSubNode = b.node;
				while (bSubNode.type === 'tagger')
				{
					nesting = true;
	
					typeof bTaggers !== 'object'
						? bTaggers = [bTaggers, bSubNode.tagger]
						: bTaggers.push(bSubNode.tagger);
	
					bSubNode = bSubNode.node;
				}
	
				// Just bail if different numbers of taggers. This implies the
				// structure of the virtual DOM has changed.
				if (nesting && aTaggers.length !== bTaggers.length)
				{
					patches.push(makePatch('p-redraw', index, b));
					return;
				}
	
				// check if taggers are "the same"
				if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
				{
					patches.push(makePatch('p-tagger', index, bTaggers));
				}
	
				// diff everything below the taggers
				diffHelp(aSubNode, bSubNode, patches, index + 1);
				return;
	
			case 'text':
				if (a.text !== b.text)
				{
					patches.push(makePatch('p-text', index, b.text));
					return;
				}
	
				return;
	
			case 'node':
				// Bail if obvious indicators have changed. Implies more serious
				// structural changes such that it's not worth it to diff.
				if (a.tag !== b.tag || a.namespace !== b.namespace)
				{
					patches.push(makePatch('p-redraw', index, b));
					return;
				}
	
				var factsDiff = diffFacts(a.facts, b.facts);
	
				if (typeof factsDiff !== 'undefined')
				{
					patches.push(makePatch('p-facts', index, factsDiff));
				}
	
				diffChildren(a, b, patches, index);
				return;
	
			case 'keyed-node':
				// Bail if obvious indicators have changed. Implies more serious
				// structural changes such that it's not worth it to diff.
				if (a.tag !== b.tag || a.namespace !== b.namespace)
				{
					patches.push(makePatch('p-redraw', index, b));
					return;
				}
	
				var factsDiff = diffFacts(a.facts, b.facts);
	
				if (typeof factsDiff !== 'undefined')
				{
					patches.push(makePatch('p-facts', index, factsDiff));
				}
	
				diffKeyedChildren(a, b, patches, index);
				return;
	
			case 'custom':
				if (a.impl !== b.impl)
				{
					patches.push(makePatch('p-redraw', index, b));
					return;
				}
	
				var factsDiff = diffFacts(a.facts, b.facts);
				if (typeof factsDiff !== 'undefined')
				{
					patches.push(makePatch('p-facts', index, factsDiff));
				}
	
				var patch = b.impl.diff(a,b);
				if (patch)
				{
					patches.push(makePatch('p-custom', index, patch));
					return;
				}
	
				return;
		}
	}
	
	
	// assumes the incoming arrays are the same length
	function pairwiseRefEqual(as, bs)
	{
		for (var i = 0; i < as.length; i++)
		{
			if (as[i] !== bs[i])
			{
				return false;
			}
		}
	
		return true;
	}
	
	
	// TODO Instead of creating a new diff object, it's possible to just test if
	// there *is* a diff. During the actual patch, do the diff again and make the
	// modifications directly. This way, there's no new allocations. Worth it?
	function diffFacts(a, b, category)
	{
		var diff;
	
		// look for changes and removals
		for (var aKey in a)
		{
			if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
			{
				var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
				if (subDiff)
				{
					diff = diff || {};
					diff[aKey] = subDiff;
				}
				continue;
			}
	
			// remove if not in the new facts
			if (!(aKey in b))
			{
				diff = diff || {};
				diff[aKey] =
					(typeof category === 'undefined')
						? (typeof a[aKey] === 'string' ? '' : null)
						:
					(category === STYLE_KEY)
						? ''
						:
					(category === EVENT_KEY || category === ATTR_KEY)
						? undefined
						:
					{ namespace: a[aKey].namespace, value: undefined };
	
				continue;
			}
	
			var aValue = a[aKey];
			var bValue = b[aKey];
	
			// reference equal, so don't worry about it
			if (aValue === bValue && aKey !== 'value'
				|| category === EVENT_KEY && equalEvents(aValue, bValue))
			{
				continue;
			}
	
			diff = diff || {};
			diff[aKey] = bValue;
		}
	
		// add new stuff
		for (var bKey in b)
		{
			if (!(bKey in a))
			{
				diff = diff || {};
				diff[bKey] = b[bKey];
			}
		}
	
		return diff;
	}
	
	
	function diffChildren(aParent, bParent, patches, rootIndex)
	{
		var aChildren = aParent.children;
		var bChildren = bParent.children;
	
		var aLen = aChildren.length;
		var bLen = bChildren.length;
	
		// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS
	
		if (aLen > bLen)
		{
			patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
		}
		else if (aLen < bLen)
		{
			patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
		}
	
		// PAIRWISE DIFF EVERYTHING ELSE
	
		var index = rootIndex;
		var minLen = aLen < bLen ? aLen : bLen;
		for (var i = 0; i < minLen; i++)
		{
			index++;
			var aChild = aChildren[i];
			diffHelp(aChild, bChildren[i], patches, index);
			index += aChild.descendantsCount || 0;
		}
	}
	
	
	
	////////////  KEYED DIFF  ////////////
	
	
	function diffKeyedChildren(aParent, bParent, patches, rootIndex)
	{
		var localPatches = [];
	
		var changes = {}; // Dict String Entry
		var inserts = []; // Array { index : Int, entry : Entry }
		// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }
	
		var aChildren = aParent.children;
		var bChildren = bParent.children;
		var aLen = aChildren.length;
		var bLen = bChildren.length;
		var aIndex = 0;
		var bIndex = 0;
	
		var index = rootIndex;
	
		while (aIndex < aLen && bIndex < bLen)
		{
			var a = aChildren[aIndex];
			var b = bChildren[bIndex];
	
			var aKey = a._0;
			var bKey = b._0;
			var aNode = a._1;
			var bNode = b._1;
	
			// check if keys match
	
			if (aKey === bKey)
			{
				index++;
				diffHelp(aNode, bNode, localPatches, index);
				index += aNode.descendantsCount || 0;
	
				aIndex++;
				bIndex++;
				continue;
			}
	
			// look ahead 1 to detect insertions and removals.
	
			var aLookAhead = aIndex + 1 < aLen;
			var bLookAhead = bIndex + 1 < bLen;
	
			if (aLookAhead)
			{
				var aNext = aChildren[aIndex + 1];
				var aNextKey = aNext._0;
				var aNextNode = aNext._1;
				var oldMatch = bKey === aNextKey;
			}
	
			if (bLookAhead)
			{
				var bNext = bChildren[bIndex + 1];
				var bNextKey = bNext._0;
				var bNextNode = bNext._1;
				var newMatch = aKey === bNextKey;
			}
	
	
			// swap a and b
			if (aLookAhead && bLookAhead && newMatch && oldMatch)
			{
				index++;
				diffHelp(aNode, bNextNode, localPatches, index);
				insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
				index += aNode.descendantsCount || 0;
	
				index++;
				removeNode(changes, localPatches, aKey, aNextNode, index);
				index += aNextNode.descendantsCount || 0;
	
				aIndex += 2;
				bIndex += 2;
				continue;
			}
	
			// insert b
			if (bLookAhead && newMatch)
			{
				index++;
				insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
				diffHelp(aNode, bNextNode, localPatches, index);
				index += aNode.descendantsCount || 0;
	
				aIndex += 1;
				bIndex += 2;
				continue;
			}
	
			// remove a
			if (aLookAhead && oldMatch)
			{
				index++;
				removeNode(changes, localPatches, aKey, aNode, index);
				index += aNode.descendantsCount || 0;
	
				index++;
				diffHelp(aNextNode, bNode, localPatches, index);
				index += aNextNode.descendantsCount || 0;
	
				aIndex += 2;
				bIndex += 1;
				continue;
			}
	
			// remove a, insert b
			if (aLookAhead && bLookAhead && aNextKey === bNextKey)
			{
				index++;
				removeNode(changes, localPatches, aKey, aNode, index);
				insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
				index += aNode.descendantsCount || 0;
	
				index++;
				diffHelp(aNextNode, bNextNode, localPatches, index);
				index += aNextNode.descendantsCount || 0;
	
				aIndex += 2;
				bIndex += 2;
				continue;
			}
	
			break;
		}
	
		// eat up any remaining nodes with removeNode and insertNode
	
		while (aIndex < aLen)
		{
			index++;
			var a = aChildren[aIndex];
			var aNode = a._1;
			removeNode(changes, localPatches, a._0, aNode, index);
			index += aNode.descendantsCount || 0;
			aIndex++;
		}
	
		var endInserts;
		while (bIndex < bLen)
		{
			endInserts = endInserts || [];
			var b = bChildren[bIndex];
			insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
			bIndex++;
		}
	
		if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
		{
			patches.push(makePatch('p-reorder', rootIndex, {
				patches: localPatches,
				inserts: inserts,
				endInserts: endInserts
			}));
		}
	}
	
	
	
	////////////  CHANGES FROM KEYED DIFF  ////////////
	
	
	var POSTFIX = '_elmW6BL';
	
	
	function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
	{
		var entry = changes[key];
	
		// never seen this key before
		if (typeof entry === 'undefined')
		{
			entry = {
				tag: 'insert',
				vnode: vnode,
				index: bIndex,
				data: undefined
			};
	
			inserts.push({ index: bIndex, entry: entry });
			changes[key] = entry;
	
			return;
		}
	
		// this key was removed earlier, a match!
		if (entry.tag === 'remove')
		{
			inserts.push({ index: bIndex, entry: entry });
	
			entry.tag = 'move';
			var subPatches = [];
			diffHelp(entry.vnode, vnode, subPatches, entry.index);
			entry.index = bIndex;
			entry.data.data = {
				patches: subPatches,
				entry: entry
			};
	
			return;
		}
	
		// this key has already been inserted or moved, a duplicate!
		insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
	}
	
	
	function removeNode(changes, localPatches, key, vnode, index)
	{
		var entry = changes[key];
	
		// never seen this key before
		if (typeof entry === 'undefined')
		{
			var patch = makePatch('p-remove', index, undefined);
			localPatches.push(patch);
	
			changes[key] = {
				tag: 'remove',
				vnode: vnode,
				index: index,
				data: patch
			};
	
			return;
		}
	
		// this key was inserted earlier, a match!
		if (entry.tag === 'insert')
		{
			entry.tag = 'move';
			var subPatches = [];
			diffHelp(vnode, entry.vnode, subPatches, index);
	
			var patch = makePatch('p-remove', index, {
				patches: subPatches,
				entry: entry
			});
			localPatches.push(patch);
	
			return;
		}
	
		// this key has already been removed or moved, a duplicate!
		removeNode(changes, localPatches, key + POSTFIX, vnode, index);
	}
	
	
	
	////////////  ADD DOM NODES  ////////////
	//
	// Each DOM node has an "index" assigned in order of traversal. It is important
	// to minimize our crawl over the actual DOM, so these indexes (along with the
	// descendantsCount of virtual nodes) let us skip touching entire subtrees of
	// the DOM if we know there are no patches there.
	
	
	function addDomNodes(domNode, vNode, patches, eventNode)
	{
		addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
	}
	
	
	// assumes `patches` is non-empty and indexes increase monotonically.
	function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
	{
		var patch = patches[i];
		var index = patch.index;
	
		while (index === low)
		{
			var patchType = patch.type;
	
			if (patchType === 'p-thunk')
			{
				addDomNodes(domNode, vNode.node, patch.data, eventNode);
			}
			else if (patchType === 'p-reorder')
			{
				patch.domNode = domNode;
				patch.eventNode = eventNode;
	
				var subPatches = patch.data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
			else if (patchType === 'p-remove')
			{
				patch.domNode = domNode;
				patch.eventNode = eventNode;
	
				var data = patch.data;
				if (typeof data !== 'undefined')
				{
					data.entry.data = domNode;
					var subPatches = data.patches;
					if (subPatches.length > 0)
					{
						addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
					}
				}
			}
			else
			{
				patch.domNode = domNode;
				patch.eventNode = eventNode;
			}
	
			i++;
	
			if (!(patch = patches[i]) || (index = patch.index) > high)
			{
				return i;
			}
		}
	
		switch (vNode.type)
		{
			case 'tagger':
				var subNode = vNode.node;
	
				while (subNode.type === "tagger")
				{
					subNode = subNode.node;
				}
	
				return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	
			case 'node':
				var vChildren = vNode.children;
				var childNodes = domNode.childNodes;
				for (var j = 0; j < vChildren.length; j++)
				{
					low++;
					var vChild = vChildren[j];
					var nextLow = low + (vChild.descendantsCount || 0);
					if (low <= index && index <= nextLow)
					{
						i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
						if (!(patch = patches[i]) || (index = patch.index) > high)
						{
							return i;
						}
					}
					low = nextLow;
				}
				return i;
	
			case 'keyed-node':
				var vChildren = vNode.children;
				var childNodes = domNode.childNodes;
				for (var j = 0; j < vChildren.length; j++)
				{
					low++;
					var vChild = vChildren[j]._1;
					var nextLow = low + (vChild.descendantsCount || 0);
					if (low <= index && index <= nextLow)
					{
						i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
						if (!(patch = patches[i]) || (index = patch.index) > high)
						{
							return i;
						}
					}
					low = nextLow;
				}
				return i;
	
			case 'text':
			case 'thunk':
				throw new Error('should never traverse `text` or `thunk` nodes like this');
		}
	}
	
	
	
	////////////  APPLY PATCHES  ////////////
	
	
	function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
	{
		if (patches.length === 0)
		{
			return rootDomNode;
		}
	
		addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
		return applyPatchesHelp(rootDomNode, patches);
	}
	
	function applyPatchesHelp(rootDomNode, patches)
	{
		for (var i = 0; i < patches.length; i++)
		{
			var patch = patches[i];
			var localDomNode = patch.domNode
			var newNode = applyPatch(localDomNode, patch);
			if (localDomNode === rootDomNode)
			{
				rootDomNode = newNode;
			}
		}
		return rootDomNode;
	}
	
	function applyPatch(domNode, patch)
	{
		switch (patch.type)
		{
			case 'p-redraw':
				return redraw(domNode, patch.data, patch.eventNode);
	
			case 'p-facts':
				applyFacts(domNode, patch.eventNode, patch.data);
				return domNode;
	
			case 'p-text':
				domNode.replaceData(0, domNode.length, patch.data);
				return domNode;
	
			case 'p-thunk':
				return applyPatchesHelp(domNode, patch.data);
	
			case 'p-tagger':
				domNode.elm_event_node_ref.tagger = patch.data;
				return domNode;
	
			case 'p-remove-last':
				var i = patch.data;
				while (i--)
				{
					domNode.removeChild(domNode.lastChild);
				}
				return domNode;
	
			case 'p-append':
				var newNodes = patch.data;
				for (var i = 0; i < newNodes.length; i++)
				{
					domNode.appendChild(render(newNodes[i], patch.eventNode));
				}
				return domNode;
	
			case 'p-remove':
				var data = patch.data;
				if (typeof data === 'undefined')
				{
					domNode.parentNode.removeChild(domNode);
					return domNode;
				}
				var entry = data.entry;
				if (typeof entry.index !== 'undefined')
				{
					domNode.parentNode.removeChild(domNode);
				}
				entry.data = applyPatchesHelp(domNode, data.patches);
				return domNode;
	
			case 'p-reorder':
				var data = patch.data;
	
				// end inserts
				var endInserts = data.endInserts;
				var end;
				if (typeof endInserts !== 'undefined')
				{
					if (endInserts.length === 1)
					{
						var insert = endInserts[0];
						var entry = insert.entry;
						var end = entry.tag === 'move'
							? entry.data
							: render(entry.vnode, patch.eventNode);
					}
					else
					{
						end = document.createDocumentFragment();
						for (var i = 0; i < endInserts.length; i++)
						{
							var insert = endInserts[i];
							var entry = insert.entry;
							var node = entry.tag === 'move'
								? entry.data
								: render(entry.vnode, patch.eventNode);
							end.appendChild(node);
						}
					}
				}
	
				// removals
				domNode = applyPatchesHelp(domNode, data.patches);
	
				// inserts
				var inserts = data.inserts;
				for (var i = 0; i < inserts.length; i++)
				{
					var insert = inserts[i];
					var entry = insert.entry;
					var node = entry.tag === 'move'
						? entry.data
						: render(entry.vnode, patch.eventNode);
					domNode.insertBefore(node, domNode.childNodes[insert.index]);
				}
	
				if (typeof end !== 'undefined')
				{
					domNode.appendChild(end);
				}
	
				return domNode;
	
			case 'p-custom':
				var impl = patch.data;
				return impl.applyPatch(domNode, impl.data);
	
			default:
				throw new Error('Ran into an unknown patch!');
		}
	}
	
	
	function redraw(domNode, vNode, eventNode)
	{
		var parentNode = domNode.parentNode;
		var newNode = render(vNode, eventNode);
	
		if (typeof newNode.elm_event_node_ref === 'undefined')
		{
			newNode.elm_event_node_ref = domNode.elm_event_node_ref;
		}
	
		if (parentNode && newNode !== domNode)
		{
			parentNode.replaceChild(newNode, domNode);
		}
		return newNode;
	}
	
	
	
	////////////  PROGRAMS  ////////////
	
	
	function programWithFlags(details)
	{
		return {
			init: details.init,
			update: details.update,
			subscriptions: details.subscriptions,
			view: details.view,
			renderer: renderer
		};
	}
	
	
	return {
		node: node,
		text: text,
	
		custom: custom,
	
		map: F2(map),
	
		on: F3(on),
		style: style,
		property: F2(property),
		attribute: F2(attribute),
		attributeNS: F3(attributeNS),
	
		lazy: F2(lazy),
		lazy2: F3(lazy2),
		lazy3: F4(lazy3),
		keyedNode: F3(keyedNode),
	
		programWithFlags: programWithFlags
	};
	
	}();
	var _elm_lang$virtual_dom$VirtualDom$programWithFlags = _elm_lang$virtual_dom$Native_VirtualDom.programWithFlags;
	var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
	var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
	var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
	var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
	var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
	var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
	var _elm_lang$virtual_dom$VirtualDom$on = F2(
		function (eventName, decoder) {
			return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
		});
	var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
	var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
	var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
	var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
	var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
	var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
	var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
	var _elm_lang$virtual_dom$VirtualDom$Options = F2(
		function (a, b) {
			return {stopPropagation: a, preventDefault: b};
		});
	var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
	var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};
	
	var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
	var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
	var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
	var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
	var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
	var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
	var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
	var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
	var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
	var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
	var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
	var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
	var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
	var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
	var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
	var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
	var _elm_lang$html$Html$main$ = _elm_lang$html$Html$node('main');
	var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
	var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
	var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
	var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
	var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
	var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
	var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
	var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
	var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
	var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
	var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
	var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
	var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
	var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
	var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
	var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
	var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
	var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
	var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
	var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
	var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
	var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
	var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
	var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
	var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
	var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
	var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
	var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
	var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
	var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
	var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
	var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
	var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
	var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
	var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
	var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
	var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
	var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
	var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
	var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
	var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
	var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
	var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
	var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
	var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
	var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
	var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
	var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
	var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
	var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
	var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
	var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
	var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
	var _elm_lang$html$Html$svg = _elm_lang$html$Html$node('svg');
	var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
	var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
	var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
	var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
	var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
	var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
	var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
	var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
	var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
	var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
	var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
	var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
	var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
	var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
	var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
	var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
	var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
	var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
	var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
	var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
	var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
	var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
	var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
	var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
	var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
	var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
	var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
	var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
	var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
	var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');
	
	var _elm_lang$html$Html_App$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
	var _elm_lang$html$Html_App$program = function (app) {
		return _elm_lang$html$Html_App$programWithFlags(
			_elm_lang$core$Native_Utils.update(
				app,
				{
					init: function (_p0) {
						return app.init;
					}
				}));
	};
	var _elm_lang$html$Html_App$beginnerProgram = function (_p1) {
		var _p2 = _p1;
		return _elm_lang$html$Html_App$programWithFlags(
			{
				init: function (_p3) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_p2.model,
						_elm_lang$core$Native_List.fromArray(
							[]));
				},
				update: F2(
					function (msg, model) {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							A2(_p2.update, msg, model),
							_elm_lang$core$Native_List.fromArray(
								[]));
					}),
				view: _p2.view,
				subscriptions: function (_p4) {
					return _elm_lang$core$Platform_Sub$none;
				}
			});
	};
	var _elm_lang$html$Html_App$map = _elm_lang$virtual_dom$VirtualDom$map;
	
	var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
	var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
		return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
	};
	var _elm_lang$html$Html_Attributes$draggable = function (value) {
		return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
	};
	var _elm_lang$html$Html_Attributes$list = function (value) {
		return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
	};
	var _elm_lang$html$Html_Attributes$maxlength = function (n) {
		return A2(
			_elm_lang$html$Html_Attributes$attribute,
			'maxlength',
			_elm_lang$core$Basics$toString(n));
	};
	var _elm_lang$html$Html_Attributes$datetime = function (value) {
		return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
	};
	var _elm_lang$html$Html_Attributes$pubdate = function (value) {
		return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
	};
	var _elm_lang$html$Html_Attributes$colspan = function (n) {
		return A2(
			_elm_lang$html$Html_Attributes$attribute,
			'colspan',
			_elm_lang$core$Basics$toString(n));
	};
	var _elm_lang$html$Html_Attributes$rowspan = function (n) {
		return A2(
			_elm_lang$html$Html_Attributes$attribute,
			'rowspan',
			_elm_lang$core$Basics$toString(n));
	};
	var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
	var _elm_lang$html$Html_Attributes$stringProperty = F2(
		function (name, string) {
			return A2(
				_elm_lang$html$Html_Attributes$property,
				name,
				_elm_lang$core$Json_Encode$string(string));
		});
	var _elm_lang$html$Html_Attributes$class = function (name) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
	};
	var _elm_lang$html$Html_Attributes$id = function (name) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
	};
	var _elm_lang$html$Html_Attributes$title = function (name) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
	};
	var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
		return A2(
			_elm_lang$html$Html_Attributes$stringProperty,
			'accessKey',
			_elm_lang$core$String$fromChar($char));
	};
	var _elm_lang$html$Html_Attributes$dir = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
	};
	var _elm_lang$html$Html_Attributes$dropzone = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
	};
	var _elm_lang$html$Html_Attributes$itemprop = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'itemprop', value);
	};
	var _elm_lang$html$Html_Attributes$lang = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
	};
	var _elm_lang$html$Html_Attributes$tabindex = function (n) {
		return A2(
			_elm_lang$html$Html_Attributes$stringProperty,
			'tabIndex',
			_elm_lang$core$Basics$toString(n));
	};
	var _elm_lang$html$Html_Attributes$charset = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'charset', value);
	};
	var _elm_lang$html$Html_Attributes$content = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
	};
	var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
	};
	var _elm_lang$html$Html_Attributes$language = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
	};
	var _elm_lang$html$Html_Attributes$src = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
	};
	var _elm_lang$html$Html_Attributes$height = function (value) {
		return A2(
			_elm_lang$html$Html_Attributes$stringProperty,
			'height',
			_elm_lang$core$Basics$toString(value));
	};
	var _elm_lang$html$Html_Attributes$width = function (value) {
		return A2(
			_elm_lang$html$Html_Attributes$stringProperty,
			'width',
			_elm_lang$core$Basics$toString(value));
	};
	var _elm_lang$html$Html_Attributes$alt = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
	};
	var _elm_lang$html$Html_Attributes$preload = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
	};
	var _elm_lang$html$Html_Attributes$poster = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
	};
	var _elm_lang$html$Html_Attributes$kind = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
	};
	var _elm_lang$html$Html_Attributes$srclang = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
	};
	var _elm_lang$html$Html_Attributes$sandbox = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
	};
	var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
	};
	var _elm_lang$html$Html_Attributes$type$ = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
	};
	var _elm_lang$html$Html_Attributes$value = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
	};
	var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
	};
	var _elm_lang$html$Html_Attributes$placeholder = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
	};
	var _elm_lang$html$Html_Attributes$accept = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
	};
	var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
	};
	var _elm_lang$html$Html_Attributes$action = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
	};
	var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
		return A2(
			_elm_lang$html$Html_Attributes$stringProperty,
			'autocomplete',
			bool ? 'on' : 'off');
	};
	var _elm_lang$html$Html_Attributes$autosave = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'autosave', value);
	};
	var _elm_lang$html$Html_Attributes$enctype = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
	};
	var _elm_lang$html$Html_Attributes$formaction = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'formAction', value);
	};
	var _elm_lang$html$Html_Attributes$minlength = function (n) {
		return A2(
			_elm_lang$html$Html_Attributes$stringProperty,
			'minLength',
			_elm_lang$core$Basics$toString(n));
	};
	var _elm_lang$html$Html_Attributes$method = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
	};
	var _elm_lang$html$Html_Attributes$name = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
	};
	var _elm_lang$html$Html_Attributes$pattern = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
	};
	var _elm_lang$html$Html_Attributes$size = function (n) {
		return A2(
			_elm_lang$html$Html_Attributes$stringProperty,
			'size',
			_elm_lang$core$Basics$toString(n));
	};
	var _elm_lang$html$Html_Attributes$for = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
	};
	var _elm_lang$html$Html_Attributes$form = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'form', value);
	};
	var _elm_lang$html$Html_Attributes$max = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
	};
	var _elm_lang$html$Html_Attributes$min = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
	};
	var _elm_lang$html$Html_Attributes$step = function (n) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
	};
	var _elm_lang$html$Html_Attributes$cols = function (n) {
		return A2(
			_elm_lang$html$Html_Attributes$stringProperty,
			'cols',
			_elm_lang$core$Basics$toString(n));
	};
	var _elm_lang$html$Html_Attributes$rows = function (n) {
		return A2(
			_elm_lang$html$Html_Attributes$stringProperty,
			'rows',
			_elm_lang$core$Basics$toString(n));
	};
	var _elm_lang$html$Html_Attributes$wrap = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
	};
	var _elm_lang$html$Html_Attributes$usemap = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
	};
	var _elm_lang$html$Html_Attributes$shape = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
	};
	var _elm_lang$html$Html_Attributes$coords = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
	};
	var _elm_lang$html$Html_Attributes$challenge = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'challenge', value);
	};
	var _elm_lang$html$Html_Attributes$keytype = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
	};
	var _elm_lang$html$Html_Attributes$align = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
	};
	var _elm_lang$html$Html_Attributes$cite = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
	};
	var _elm_lang$html$Html_Attributes$href = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
	};
	var _elm_lang$html$Html_Attributes$target = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
	};
	var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
	};
	var _elm_lang$html$Html_Attributes$hreflang = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
	};
	var _elm_lang$html$Html_Attributes$media = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'media', value);
	};
	var _elm_lang$html$Html_Attributes$ping = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
	};
	var _elm_lang$html$Html_Attributes$rel = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'rel', value);
	};
	var _elm_lang$html$Html_Attributes$start = function (n) {
		return A2(
			_elm_lang$html$Html_Attributes$stringProperty,
			'start',
			_elm_lang$core$Basics$toString(n));
	};
	var _elm_lang$html$Html_Attributes$headers = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
	};
	var _elm_lang$html$Html_Attributes$scope = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
	};
	var _elm_lang$html$Html_Attributes$manifest = function (value) {
		return A2(_elm_lang$html$Html_Attributes$stringProperty, 'manifest', value);
	};
	var _elm_lang$html$Html_Attributes$boolProperty = F2(
		function (name, bool) {
			return A2(
				_elm_lang$html$Html_Attributes$property,
				name,
				_elm_lang$core$Json_Encode$bool(bool));
		});
	var _elm_lang$html$Html_Attributes$hidden = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
	};
	var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
	};
	var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
	};
	var _elm_lang$html$Html_Attributes$async = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
	};
	var _elm_lang$html$Html_Attributes$defer = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
	};
	var _elm_lang$html$Html_Attributes$scoped = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
	};
	var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
	};
	var _elm_lang$html$Html_Attributes$controls = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
	};
	var _elm_lang$html$Html_Attributes$loop = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
	};
	var _elm_lang$html$Html_Attributes$default = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
	};
	var _elm_lang$html$Html_Attributes$seamless = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
	};
	var _elm_lang$html$Html_Attributes$checked = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
	};
	var _elm_lang$html$Html_Attributes$selected = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
	};
	var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
	};
	var _elm_lang$html$Html_Attributes$disabled = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
	};
	var _elm_lang$html$Html_Attributes$multiple = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
	};
	var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
	};
	var _elm_lang$html$Html_Attributes$readonly = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
	};
	var _elm_lang$html$Html_Attributes$required = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
	};
	var _elm_lang$html$Html_Attributes$ismap = function (value) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
	};
	var _elm_lang$html$Html_Attributes$download = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
	};
	var _elm_lang$html$Html_Attributes$reversed = function (bool) {
		return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
	};
	var _elm_lang$html$Html_Attributes$classList = function (list) {
		return _elm_lang$html$Html_Attributes$class(
			A2(
				_elm_lang$core$String$join,
				' ',
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Basics$fst,
					A2(_elm_lang$core$List$filter, _elm_lang$core$Basics$snd, list))));
	};
	var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;
	
	var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode_ops[':='], 'keyCode', _elm_lang$core$Json_Decode$int);
	var _elm_lang$html$Html_Events$targetChecked = A2(
		_elm_lang$core$Json_Decode$at,
		_elm_lang$core$Native_List.fromArray(
			['target', 'checked']),
		_elm_lang$core$Json_Decode$bool);
	var _elm_lang$html$Html_Events$targetValue = A2(
		_elm_lang$core$Json_Decode$at,
		_elm_lang$core$Native_List.fromArray(
			['target', 'value']),
		_elm_lang$core$Json_Decode$string);
	var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
	var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
	var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
	var _elm_lang$html$Html_Events$onFocus = function (msg) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'focus',
			_elm_lang$core$Json_Decode$succeed(msg));
	};
	var _elm_lang$html$Html_Events$onBlur = function (msg) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'blur',
			_elm_lang$core$Json_Decode$succeed(msg));
	};
	var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
		_elm_lang$html$Html_Events$defaultOptions,
		{preventDefault: true});
	var _elm_lang$html$Html_Events$onSubmit = function (msg) {
		return A3(
			_elm_lang$html$Html_Events$onWithOptions,
			'submit',
			_elm_lang$html$Html_Events$onSubmitOptions,
			_elm_lang$core$Json_Decode$succeed(msg));
	};
	var _elm_lang$html$Html_Events$onCheck = function (tagger) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'change',
			A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
	};
	var _elm_lang$html$Html_Events$onInput = function (tagger) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'input',
			A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
	};
	var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'mouseout',
			_elm_lang$core$Json_Decode$succeed(msg));
	};
	var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'mouseover',
			_elm_lang$core$Json_Decode$succeed(msg));
	};
	var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'mouseleave',
			_elm_lang$core$Json_Decode$succeed(msg));
	};
	var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'mouseenter',
			_elm_lang$core$Json_Decode$succeed(msg));
	};
	var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'mouseup',
			_elm_lang$core$Json_Decode$succeed(msg));
	};
	var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'mousedown',
			_elm_lang$core$Json_Decode$succeed(msg));
	};
	var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'dblclick',
			_elm_lang$core$Json_Decode$succeed(msg));
	};
	var _elm_lang$html$Html_Events$onClick = function (msg) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'click',
			_elm_lang$core$Json_Decode$succeed(msg));
	};
	var _elm_lang$html$Html_Events$Options = F2(
		function (a, b) {
			return {stopPropagation: a, preventDefault: b};
		});
	
	var _elm_lang$keyboard$Keyboard$onSelfMsg = F3(
		function (router, _p0, state) {
			var _p1 = _p0;
			var _p2 = A2(_elm_lang$core$Dict$get, _p1.category, state);
			if (_p2.ctor === 'Nothing') {
				return _elm_lang$core$Task$succeed(state);
			} else {
				var send = function (tagger) {
					return A2(
						_elm_lang$core$Platform$sendToApp,
						router,
						tagger(_p1.keyCode));
				};
				return A2(
					_elm_lang$core$Task$andThen,
					_elm_lang$core$Task$sequence(
						A2(_elm_lang$core$List$map, send, _p2._0.taggers)),
					function (_p3) {
						return _elm_lang$core$Task$succeed(state);
					});
			}
		});
	var _elm_lang$keyboard$Keyboard_ops = _elm_lang$keyboard$Keyboard_ops || {};
	_elm_lang$keyboard$Keyboard_ops['&>'] = F2(
		function (t1, t2) {
			return A2(
				_elm_lang$core$Task$andThen,
				t1,
				function (_p4) {
					return t2;
				});
		});
	var _elm_lang$keyboard$Keyboard$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
	var _elm_lang$keyboard$Keyboard$categorizeHelpHelp = F2(
		function (value, maybeValues) {
			var _p5 = maybeValues;
			if (_p5.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Native_List.fromArray(
						[value]));
			} else {
				return _elm_lang$core$Maybe$Just(
					A2(_elm_lang$core$List_ops['::'], value, _p5._0));
			}
		});
	var _elm_lang$keyboard$Keyboard$categorizeHelp = F2(
		function (subs, subDict) {
			categorizeHelp:
			while (true) {
				var _p6 = subs;
				if (_p6.ctor === '[]') {
					return subDict;
				} else {
					var _v4 = _p6._1,
						_v5 = A3(
						_elm_lang$core$Dict$update,
						_p6._0._0,
						_elm_lang$keyboard$Keyboard$categorizeHelpHelp(_p6._0._1),
						subDict);
					subs = _v4;
					subDict = _v5;
					continue categorizeHelp;
				}
			}
		});
	var _elm_lang$keyboard$Keyboard$categorize = function (subs) {
		return A2(_elm_lang$keyboard$Keyboard$categorizeHelp, subs, _elm_lang$core$Dict$empty);
	};
	var _elm_lang$keyboard$Keyboard$keyCode = A2(_elm_lang$core$Json_Decode_ops[':='], 'keyCode', _elm_lang$core$Json_Decode$int);
	var _elm_lang$keyboard$Keyboard$subscription = _elm_lang$core$Native_Platform.leaf('Keyboard');
	var _elm_lang$keyboard$Keyboard$Watcher = F2(
		function (a, b) {
			return {taggers: a, pid: b};
		});
	var _elm_lang$keyboard$Keyboard$Msg = F2(
		function (a, b) {
			return {category: a, keyCode: b};
		});
	var _elm_lang$keyboard$Keyboard$onEffects = F3(
		function (router, newSubs, oldState) {
			var rightStep = F3(
				function (category, taggers, task) {
					return A2(
						_elm_lang$core$Task$andThen,
						task,
						function (state) {
							return A2(
								_elm_lang$core$Task$andThen,
								_elm_lang$core$Process$spawn(
									A3(
										_elm_lang$dom$Dom_LowLevel$onDocument,
										category,
										_elm_lang$keyboard$Keyboard$keyCode,
										function (_p7) {
											return A2(
												_elm_lang$core$Platform$sendToSelf,
												router,
												A2(_elm_lang$keyboard$Keyboard$Msg, category, _p7));
										})),
								function (pid) {
									return _elm_lang$core$Task$succeed(
										A3(
											_elm_lang$core$Dict$insert,
											category,
											A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, pid),
											state));
								});
						});
				});
			var bothStep = F4(
				function (category, _p8, taggers, task) {
					var _p9 = _p8;
					return A2(
						_elm_lang$core$Task$andThen,
						task,
						function (state) {
							return _elm_lang$core$Task$succeed(
								A3(
									_elm_lang$core$Dict$insert,
									category,
									A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, _p9.pid),
									state));
						});
				});
			var leftStep = F3(
				function (category, _p10, task) {
					var _p11 = _p10;
					return A2(
						_elm_lang$keyboard$Keyboard_ops['&>'],
						_elm_lang$core$Process$kill(_p11.pid),
						task);
				});
			return A6(
				_elm_lang$core$Dict$merge,
				leftStep,
				bothStep,
				rightStep,
				oldState,
				_elm_lang$keyboard$Keyboard$categorize(newSubs),
				_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
		});
	var _elm_lang$keyboard$Keyboard$MySub = F2(
		function (a, b) {
			return {ctor: 'MySub', _0: a, _1: b};
		});
	var _elm_lang$keyboard$Keyboard$presses = function (tagger) {
		return _elm_lang$keyboard$Keyboard$subscription(
			A2(_elm_lang$keyboard$Keyboard$MySub, 'keypress', tagger));
	};
	var _elm_lang$keyboard$Keyboard$downs = function (tagger) {
		return _elm_lang$keyboard$Keyboard$subscription(
			A2(_elm_lang$keyboard$Keyboard$MySub, 'keydown', tagger));
	};
	var _elm_lang$keyboard$Keyboard$ups = function (tagger) {
		return _elm_lang$keyboard$Keyboard$subscription(
			A2(_elm_lang$keyboard$Keyboard$MySub, 'keyup', tagger));
	};
	var _elm_lang$keyboard$Keyboard$subMap = F2(
		function (func, _p12) {
			var _p13 = _p12;
			return A2(
				_elm_lang$keyboard$Keyboard$MySub,
				_p13._0,
				function (_p14) {
					return func(
						_p13._1(_p14));
				});
		});
	_elm_lang$core$Native_Platform.effectManagers['Keyboard'] = {pkg: 'elm-lang/keyboard', init: _elm_lang$keyboard$Keyboard$init, onEffects: _elm_lang$keyboard$Keyboard$onEffects, onSelfMsg: _elm_lang$keyboard$Keyboard$onSelfMsg, tag: 'sub', subMap: _elm_lang$keyboard$Keyboard$subMap};
	
	var _elm_lang$mouse$Mouse$onSelfMsg = F3(
		function (router, _p0, state) {
			var _p1 = _p0;
			var _p2 = A2(_elm_lang$core$Dict$get, _p1.category, state);
			if (_p2.ctor === 'Nothing') {
				return _elm_lang$core$Task$succeed(state);
			} else {
				var send = function (tagger) {
					return A2(
						_elm_lang$core$Platform$sendToApp,
						router,
						tagger(_p1.position));
				};
				return A2(
					_elm_lang$core$Task$andThen,
					_elm_lang$core$Task$sequence(
						A2(_elm_lang$core$List$map, send, _p2._0.taggers)),
					function (_p3) {
						return _elm_lang$core$Task$succeed(state);
					});
			}
		});
	var _elm_lang$mouse$Mouse_ops = _elm_lang$mouse$Mouse_ops || {};
	_elm_lang$mouse$Mouse_ops['&>'] = F2(
		function (t1, t2) {
			return A2(
				_elm_lang$core$Task$andThen,
				t1,
				function (_p4) {
					return t2;
				});
		});
	var _elm_lang$mouse$Mouse$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
	var _elm_lang$mouse$Mouse$categorizeHelpHelp = F2(
		function (value, maybeValues) {
			var _p5 = maybeValues;
			if (_p5.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Native_List.fromArray(
						[value]));
			} else {
				return _elm_lang$core$Maybe$Just(
					A2(_elm_lang$core$List_ops['::'], value, _p5._0));
			}
		});
	var _elm_lang$mouse$Mouse$categorizeHelp = F2(
		function (subs, subDict) {
			categorizeHelp:
			while (true) {
				var _p6 = subs;
				if (_p6.ctor === '[]') {
					return subDict;
				} else {
					var _v4 = _p6._1,
						_v5 = A3(
						_elm_lang$core$Dict$update,
						_p6._0._0,
						_elm_lang$mouse$Mouse$categorizeHelpHelp(_p6._0._1),
						subDict);
					subs = _v4;
					subDict = _v5;
					continue categorizeHelp;
				}
			}
		});
	var _elm_lang$mouse$Mouse$categorize = function (subs) {
		return A2(_elm_lang$mouse$Mouse$categorizeHelp, subs, _elm_lang$core$Dict$empty);
	};
	var _elm_lang$mouse$Mouse$subscription = _elm_lang$core$Native_Platform.leaf('Mouse');
	var _elm_lang$mouse$Mouse$Position = F2(
		function (a, b) {
			return {x: a, y: b};
		});
	var _elm_lang$mouse$Mouse$position = A3(
		_elm_lang$core$Json_Decode$object2,
		_elm_lang$mouse$Mouse$Position,
		A2(_elm_lang$core$Json_Decode_ops[':='], 'pageX', _elm_lang$core$Json_Decode$int),
		A2(_elm_lang$core$Json_Decode_ops[':='], 'pageY', _elm_lang$core$Json_Decode$int));
	var _elm_lang$mouse$Mouse$Watcher = F2(
		function (a, b) {
			return {taggers: a, pid: b};
		});
	var _elm_lang$mouse$Mouse$Msg = F2(
		function (a, b) {
			return {category: a, position: b};
		});
	var _elm_lang$mouse$Mouse$onEffects = F3(
		function (router, newSubs, oldState) {
			var rightStep = F3(
				function (category, taggers, task) {
					return A2(
						_elm_lang$core$Task$andThen,
						task,
						function (state) {
							return A2(
								_elm_lang$core$Task$andThen,
								_elm_lang$core$Process$spawn(
									A3(
										_elm_lang$dom$Dom_LowLevel$onDocument,
										category,
										_elm_lang$mouse$Mouse$position,
										function (_p7) {
											return A2(
												_elm_lang$core$Platform$sendToSelf,
												router,
												A2(_elm_lang$mouse$Mouse$Msg, category, _p7));
										})),
								function (pid) {
									return _elm_lang$core$Task$succeed(
										A3(
											_elm_lang$core$Dict$insert,
											category,
											A2(_elm_lang$mouse$Mouse$Watcher, taggers, pid),
											state));
								});
						});
				});
			var bothStep = F4(
				function (category, _p8, taggers, task) {
					var _p9 = _p8;
					return A2(
						_elm_lang$core$Task$andThen,
						task,
						function (state) {
							return _elm_lang$core$Task$succeed(
								A3(
									_elm_lang$core$Dict$insert,
									category,
									A2(_elm_lang$mouse$Mouse$Watcher, taggers, _p9.pid),
									state));
						});
				});
			var leftStep = F3(
				function (category, _p10, task) {
					var _p11 = _p10;
					return A2(
						_elm_lang$mouse$Mouse_ops['&>'],
						_elm_lang$core$Process$kill(_p11.pid),
						task);
				});
			return A6(
				_elm_lang$core$Dict$merge,
				leftStep,
				bothStep,
				rightStep,
				oldState,
				_elm_lang$mouse$Mouse$categorize(newSubs),
				_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
		});
	var _elm_lang$mouse$Mouse$MySub = F2(
		function (a, b) {
			return {ctor: 'MySub', _0: a, _1: b};
		});
	var _elm_lang$mouse$Mouse$clicks = function (tagger) {
		return _elm_lang$mouse$Mouse$subscription(
			A2(_elm_lang$mouse$Mouse$MySub, 'click', tagger));
	};
	var _elm_lang$mouse$Mouse$moves = function (tagger) {
		return _elm_lang$mouse$Mouse$subscription(
			A2(_elm_lang$mouse$Mouse$MySub, 'mousemove', tagger));
	};
	var _elm_lang$mouse$Mouse$downs = function (tagger) {
		return _elm_lang$mouse$Mouse$subscription(
			A2(_elm_lang$mouse$Mouse$MySub, 'mousedown', tagger));
	};
	var _elm_lang$mouse$Mouse$ups = function (tagger) {
		return _elm_lang$mouse$Mouse$subscription(
			A2(_elm_lang$mouse$Mouse$MySub, 'mouseup', tagger));
	};
	var _elm_lang$mouse$Mouse$subMap = F2(
		function (func, _p12) {
			var _p13 = _p12;
			return A2(
				_elm_lang$mouse$Mouse$MySub,
				_p13._0,
				function (_p14) {
					return func(
						_p13._1(_p14));
				});
		});
	_elm_lang$core$Native_Platform.effectManagers['Mouse'] = {pkg: 'elm-lang/mouse', init: _elm_lang$mouse$Mouse$init, onEffects: _elm_lang$mouse$Mouse$onEffects, onSelfMsg: _elm_lang$mouse$Mouse$onSelfMsg, tag: 'sub', subMap: _elm_lang$mouse$Mouse$subMap};
	
	var _pablobcb$elm_lead$Component_Incrementer$incrementer = F4(
		function (kind, value, up, down) {
			var label$ = _elm_lang$core$Basics$toString(kind);
			var displayClass = function () {
				var _p0 = kind;
				switch (_p0.ctor) {
					case 'Velocity':
						return 'incrementer__display';
					case 'Octave':
						return 'incrementer__display';
					default:
						return 'incrementer__display incrementer__display--patch';
				}
			}();
			return A2(
				_elm_lang$html$Html$div,
				_elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$html$Html_Attributes$class('incrementer')
					]),
				_elm_lang$core$Native_List.fromArray(
					[
						A2(
						_elm_lang$html$Html$div,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html_Attributes$class('incrementer__info')
							]),
						_elm_lang$core$Native_List.fromArray(
							[
								A2(
								_elm_lang$html$Html$label,
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html_Attributes$class('incrementer__label')
									]),
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html$text(label$)
									])),
								A2(
								_elm_lang$html$Html$div,
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html_Attributes$class(displayClass)
									]),
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html$text(value)
									]))
							])),
						A2(
						_elm_lang$html$Html$div,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html_Attributes$class('incrementer__buttons')
							]),
						_elm_lang$core$Native_List.fromArray(
							[
								A2(
								_elm_lang$html$Html$button,
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html_Attributes$class('incrementer__button'),
										_elm_lang$html$Html_Events$onClick(down)
									]),
								_elm_lang$core$Native_List.fromArray(
									[])),
								A2(
								_elm_lang$html$Html$button,
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html_Attributes$class('incrementer__button'),
										_elm_lang$html$Html_Events$onClick(up)
									]),
								_elm_lang$core$Native_List.fromArray(
									[]))
							]))
					]));
		});
	var _pablobcb$elm_lead$Component_Incrementer$Velocity = {ctor: 'Velocity'};
	var _pablobcb$elm_lead$Component_Incrementer$Patch = {ctor: 'Patch'};
	var _pablobcb$elm_lead$Component_Incrementer$Octave = {ctor: 'Octave'};
	
	var _pablobcb$elm_lead$Note$B = {ctor: 'B'};
	var _pablobcb$elm_lead$Note$Bb = {ctor: 'Bb'};
	var _pablobcb$elm_lead$Note$A = {ctor: 'A'};
	var _pablobcb$elm_lead$Note$Ab = {ctor: 'Ab'};
	var _pablobcb$elm_lead$Note$G = {ctor: 'G'};
	var _pablobcb$elm_lead$Note$Gb = {ctor: 'Gb'};
	var _pablobcb$elm_lead$Note$F = {ctor: 'F'};
	var _pablobcb$elm_lead$Note$E = {ctor: 'E'};
	var _pablobcb$elm_lead$Note$Eb = {ctor: 'Eb'};
	var _pablobcb$elm_lead$Note$D = {ctor: 'D'};
	var _pablobcb$elm_lead$Note$Db = {ctor: 'Db'};
	var _pablobcb$elm_lead$Note$C = {ctor: 'C'};
	var _pablobcb$elm_lead$Note$octaveNotes = _elm_lang$core$Native_List.fromArray(
		[_pablobcb$elm_lead$Note$C, _pablobcb$elm_lead$Note$Db, _pablobcb$elm_lead$Note$D, _pablobcb$elm_lead$Note$Eb, _pablobcb$elm_lead$Note$E, _pablobcb$elm_lead$Note$F, _pablobcb$elm_lead$Note$Gb, _pablobcb$elm_lead$Note$G, _pablobcb$elm_lead$Note$Ab, _pablobcb$elm_lead$Note$A, _pablobcb$elm_lead$Note$Bb, _pablobcb$elm_lead$Note$B]);
	
	var _pablobcb$elm_lead$Midi$midiNoteOctaves = A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$List$concat(
			A2(
				_elm_lang$core$List$map,
				function (octave) {
					return A2(_elm_lang$core$List$repeat, 12, octave);
				},
				_elm_lang$core$Native_List.range(-2, 7))),
		A2(_elm_lang$core$List$repeat, 8, 8));
	var _pablobcb$elm_lead$Midi$midiNotesDict = function () {
		var midiNotes = _elm_lang$core$Native_List.range(0, 127);
		var pianoNotes = A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$List$concat(
				A2(_elm_lang$core$List$repeat, 10, _pablobcb$elm_lead$Note$octaveNotes)),
			A2(_elm_lang$core$List$take, 8, _pablobcb$elm_lead$Note$octaveNotes));
		return _elm_lang$core$Dict$fromList(
			A4(
				_elm_lang$core$List$map3,
				F3(
					function (pianoNote, octave, midiNote) {
						return {
							ctor: '_Tuple2',
							_0: _elm_lang$core$Basics$toString(
								{ctor: '_Tuple2', _0: pianoNote, _1: octave}),
							_1: midiNote
						};
					}),
				pianoNotes,
				_pablobcb$elm_lead$Midi$midiNoteOctaves,
				midiNotes));
	}();
	var _pablobcb$elm_lead$Midi$noteToMidiNumber = function (note) {
		var _p0 = A2(
			_elm_lang$core$Dict$get,
			_elm_lang$core$Basics$toString(note),
			_pablobcb$elm_lead$Midi$midiNotesDict);
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return _elm_lang$core$Native_Utils.crashCase(
				'Midi',
				{
					start: {line: 44, column: 5},
					end: {line: 49, column: 91}
				},
				_p0)(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'noteToMidiNumber expected a valid MIDI note',
					_elm_lang$core$Basics$toString(note)));
		}
	};
	var _pablobcb$elm_lead$Midi$makeMidiMessage = F3(
		function (note, velocity, type_) {
			return A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Maybe$Just,
				_elm_lang$core$Native_List.fromArray(
					[type_, note, velocity]));
		});
	var _pablobcb$elm_lead$Midi$noteOff = 128;
	var _pablobcb$elm_lead$Midi$noteOffMessage = F2(
		function (note, velocity) {
			return A3(_pablobcb$elm_lead$Midi$makeMidiMessage, note, velocity, _pablobcb$elm_lead$Midi$noteOff);
		});
	var _pablobcb$elm_lead$Midi$noteOn = 144;
	var _pablobcb$elm_lead$Midi$noteOnMessage = F2(
		function (note, velocity) {
			return A3(_pablobcb$elm_lead$Midi$makeMidiMessage, note, velocity, _pablobcb$elm_lead$Midi$noteOn);
		});
	
	var _pablobcb$elm_lead$Preset$Preset = F6(
		function (a, b, c, d, e, f) {
			return {name: a, presetId: b, filter: c, amp: d, oscs: e, overdrive: f};
		});
	
	var _pablobcb$elm_lead$Port$midiOut = _elm_lang$core$Native_Platform.outgoingPort(
		'midiOut',
		function (v) {
			return _elm_lang$core$Native_List.toArray(v).map(
				function (v) {
					return (v.ctor === 'Nothing') ? null : v._0;
				});
		});
	var _pablobcb$elm_lead$Port$midiIn = _elm_lang$core$Native_Platform.incomingPort(
		'midiIn',
		_elm_lang$core$Json_Decode$list(
			_elm_lang$core$Json_Decode$oneOf(
				_elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
						A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, _elm_lang$core$Json_Decode$int)
					]))));
	var _pablobcb$elm_lead$Port$midiStateChange = _elm_lang$core$Native_Platform.incomingPort('midiStateChange', _elm_lang$core$Json_Decode$bool);
	var _pablobcb$elm_lead$Port$panic = _elm_lang$core$Native_Platform.incomingPort(
		'panic',
		_elm_lang$core$Json_Decode$succeed(
			{}));
	var _pablobcb$elm_lead$Port$osc1Waveform = _elm_lang$core$Native_Platform.outgoingPort(
		'osc1Waveform',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$osc2Waveform = _elm_lang$core$Native_Platform.outgoingPort(
		'osc2Waveform',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$osc2Semitone = _elm_lang$core$Native_Platform.outgoingPort(
		'osc2Semitone',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$osc2Detune = _elm_lang$core$Native_Platform.outgoingPort(
		'osc2Detune',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$osc2KbdTrack = _elm_lang$core$Native_Platform.outgoingPort(
		'osc2KbdTrack',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$oscsBalance = _elm_lang$core$Native_Platform.outgoingPort(
		'oscsBalance',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$fmAmount = _elm_lang$core$Native_Platform.outgoingPort(
		'fmAmount',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$ampVolume = _elm_lang$core$Native_Platform.outgoingPort(
		'ampVolume',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$ampAttack = _elm_lang$core$Native_Platform.outgoingPort(
		'ampAttack',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$ampDecay = _elm_lang$core$Native_Platform.outgoingPort(
		'ampDecay',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$ampSustain = _elm_lang$core$Native_Platform.outgoingPort(
		'ampSustain',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$ampRelease = _elm_lang$core$Native_Platform.outgoingPort(
		'ampRelease',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$pulseWidth = _elm_lang$core$Native_Platform.outgoingPort(
		'pulseWidth',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$filterCutoff = _elm_lang$core$Native_Platform.outgoingPort(
		'filterCutoff',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$filterQ = _elm_lang$core$Native_Platform.outgoingPort(
		'filterQ',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$filterType = _elm_lang$core$Native_Platform.outgoingPort(
		'filterType',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$filterAttack = _elm_lang$core$Native_Platform.outgoingPort(
		'filterAttack',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$filterDecay = _elm_lang$core$Native_Platform.outgoingPort(
		'filterDecay',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$filterSustain = _elm_lang$core$Native_Platform.outgoingPort(
		'filterSustain',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$filterRelease = _elm_lang$core$Native_Platform.outgoingPort(
		'filterRelease',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$filterEnvelopeAmount = _elm_lang$core$Native_Platform.outgoingPort(
		'filterEnvelopeAmount',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$overdrive = _elm_lang$core$Native_Platform.outgoingPort(
		'overdrive',
		function (v) {
			return v;
		});
	var _pablobcb$elm_lead$Port$presetChange = _elm_lang$core$Native_Platform.incomingPort(
		'presetChange',
		A2(
			_elm_lang$core$Json_Decode$andThen,
			A2(_elm_lang$core$Json_Decode_ops[':='], 'name', _elm_lang$core$Json_Decode$string),
			function (name) {
				return A2(
					_elm_lang$core$Json_Decode$andThen,
					A2(_elm_lang$core$Json_Decode_ops[':='], 'presetId', _elm_lang$core$Json_Decode$int),
					function (presetId) {
						return A2(
							_elm_lang$core$Json_Decode$andThen,
							A2(
								_elm_lang$core$Json_Decode_ops[':='],
								'filter',
								A2(
									_elm_lang$core$Json_Decode$andThen,
									A2(_elm_lang$core$Json_Decode_ops[':='], 'type_', _elm_lang$core$Json_Decode$string),
									function (type_) {
										return A2(
											_elm_lang$core$Json_Decode$andThen,
											A2(_elm_lang$core$Json_Decode_ops[':='], 'frequency', _elm_lang$core$Json_Decode$int),
											function (frequency) {
												return A2(
													_elm_lang$core$Json_Decode$andThen,
													A2(_elm_lang$core$Json_Decode_ops[':='], 'q', _elm_lang$core$Json_Decode$int),
													function (q) {
														return A2(
															_elm_lang$core$Json_Decode$andThen,
															A2(_elm_lang$core$Json_Decode_ops[':='], 'envelopeAmount', _elm_lang$core$Json_Decode$int),
															function (envelopeAmount) {
																return A2(
																	_elm_lang$core$Json_Decode$andThen,
																	A2(
																		_elm_lang$core$Json_Decode_ops[':='],
																		'adsr',
																		A2(
																			_elm_lang$core$Json_Decode$andThen,
																			A2(_elm_lang$core$Json_Decode_ops[':='], 'attack', _elm_lang$core$Json_Decode$int),
																			function (attack) {
																				return A2(
																					_elm_lang$core$Json_Decode$andThen,
																					A2(_elm_lang$core$Json_Decode_ops[':='], 'decay', _elm_lang$core$Json_Decode$int),
																					function (decay) {
																						return A2(
																							_elm_lang$core$Json_Decode$andThen,
																							A2(_elm_lang$core$Json_Decode_ops[':='], 'sustain', _elm_lang$core$Json_Decode$int),
																							function (sustain) {
																								return A2(
																									_elm_lang$core$Json_Decode$andThen,
																									A2(_elm_lang$core$Json_Decode_ops[':='], 'release', _elm_lang$core$Json_Decode$int),
																									function (release) {
																										return _elm_lang$core$Json_Decode$succeed(
																											{attack: attack, decay: decay, sustain: sustain, release: release});
																									});
																							});
																					});
																			})),
																	function (adsr) {
																		return _elm_lang$core$Json_Decode$succeed(
																			{type_: type_, frequency: frequency, q: q, envelopeAmount: envelopeAmount, adsr: adsr});
																	});
															});
													});
											});
									})),
							function (filter) {
								return A2(
									_elm_lang$core$Json_Decode$andThen,
									A2(
										_elm_lang$core$Json_Decode_ops[':='],
										'amp',
										A2(
											_elm_lang$core$Json_Decode$andThen,
											A2(
												_elm_lang$core$Json_Decode_ops[':='],
												'adsr',
												A2(
													_elm_lang$core$Json_Decode$andThen,
													A2(_elm_lang$core$Json_Decode_ops[':='], 'attack', _elm_lang$core$Json_Decode$int),
													function (attack) {
														return A2(
															_elm_lang$core$Json_Decode$andThen,
															A2(_elm_lang$core$Json_Decode_ops[':='], 'decay', _elm_lang$core$Json_Decode$int),
															function (decay) {
																return A2(
																	_elm_lang$core$Json_Decode$andThen,
																	A2(_elm_lang$core$Json_Decode_ops[':='], 'sustain', _elm_lang$core$Json_Decode$int),
																	function (sustain) {
																		return A2(
																			_elm_lang$core$Json_Decode$andThen,
																			A2(_elm_lang$core$Json_Decode_ops[':='], 'release', _elm_lang$core$Json_Decode$int),
																			function (release) {
																				return _elm_lang$core$Json_Decode$succeed(
																					{attack: attack, decay: decay, sustain: sustain, release: release});
																			});
																	});
															});
													})),
											function (adsr) {
												return A2(
													_elm_lang$core$Json_Decode$andThen,
													A2(_elm_lang$core$Json_Decode_ops[':='], 'masterVolume', _elm_lang$core$Json_Decode$int),
													function (masterVolume) {
														return _elm_lang$core$Json_Decode$succeed(
															{adsr: adsr, masterVolume: masterVolume});
													});
											})),
									function (amp) {
										return A2(
											_elm_lang$core$Json_Decode$andThen,
											A2(
												_elm_lang$core$Json_Decode_ops[':='],
												'oscs',
												A2(
													_elm_lang$core$Json_Decode$andThen,
													A2(
														_elm_lang$core$Json_Decode_ops[':='],
														'osc1',
														A2(
															_elm_lang$core$Json_Decode$andThen,
															A2(_elm_lang$core$Json_Decode_ops[':='], 'waveformType', _elm_lang$core$Json_Decode$string),
															function (waveformType) {
																return A2(
																	_elm_lang$core$Json_Decode$andThen,
																	A2(_elm_lang$core$Json_Decode_ops[':='], 'fmGain', _elm_lang$core$Json_Decode$int),
																	function (fmGain) {
																		return _elm_lang$core$Json_Decode$succeed(
																			{waveformType: waveformType, fmGain: fmGain});
																	});
															})),
													function (osc1) {
														return A2(
															_elm_lang$core$Json_Decode$andThen,
															A2(
																_elm_lang$core$Json_Decode_ops[':='],
																'osc2',
																A2(
																	_elm_lang$core$Json_Decode$andThen,
																	A2(_elm_lang$core$Json_Decode_ops[':='], 'waveformType', _elm_lang$core$Json_Decode$string),
																	function (waveformType) {
																		return A2(
																			_elm_lang$core$Json_Decode$andThen,
																			A2(_elm_lang$core$Json_Decode_ops[':='], 'semitone', _elm_lang$core$Json_Decode$int),
																			function (semitone) {
																				return A2(
																					_elm_lang$core$Json_Decode$andThen,
																					A2(_elm_lang$core$Json_Decode_ops[':='], 'detune', _elm_lang$core$Json_Decode$int),
																					function (detune) {
																						return A2(
																							_elm_lang$core$Json_Decode$andThen,
																							A2(_elm_lang$core$Json_Decode_ops[':='], 'kbdTrack', _elm_lang$core$Json_Decode$bool),
																							function (kbdTrack) {
																								return _elm_lang$core$Json_Decode$succeed(
																									{waveformType: waveformType, semitone: semitone, detune: detune, kbdTrack: kbdTrack});
																							});
																					});
																			});
																	})),
															function (osc2) {
																return A2(
																	_elm_lang$core$Json_Decode$andThen,
																	A2(_elm_lang$core$Json_Decode_ops[':='], 'pw', _elm_lang$core$Json_Decode$int),
																	function (pw) {
																		return A2(
																			_elm_lang$core$Json_Decode$andThen,
																			A2(_elm_lang$core$Json_Decode_ops[':='], 'mix', _elm_lang$core$Json_Decode$int),
																			function (mix) {
																				return _elm_lang$core$Json_Decode$succeed(
																					{osc1: osc1, osc2: osc2, pw: pw, mix: mix});
																			});
																	});
															});
													})),
											function (oscs) {
												return A2(
													_elm_lang$core$Json_Decode$andThen,
													A2(_elm_lang$core$Json_Decode_ops[':='], 'overdrive', _elm_lang$core$Json_Decode$bool),
													function (overdrive) {
														return _elm_lang$core$Json_Decode$succeed(
															{name: name, presetId: presetId, filter: filter, amp: amp, oscs: oscs, overdrive: overdrive});
													});
											});
									});
							});
					});
			}));
	var _pablobcb$elm_lead$Port$nextPreset = _elm_lang$core$Native_Platform.outgoingPort(
		'nextPreset',
		function (v) {
			return {};
		});
	var _pablobcb$elm_lead$Port$previousPreset = _elm_lang$core$Native_Platform.outgoingPort(
		'previousPreset',
		function (v) {
			return {};
		});
	
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$panic = function (model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				pressedNotes: _elm_lang$core$Native_List.fromArray(
					[]),
				midiPressedNotes: _elm_lang$core$Native_List.fromArray(
					[]),
				mousePressedNote: _elm_lang$core$Maybe$Nothing
			});
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$noteOffCommand = F2(
		function (velocity, midiNoteNumber) {
			return _pablobcb$elm_lead$Port$midiOut(
				A2(_pablobcb$elm_lead$Midi$noteOffMessage, midiNoteNumber, velocity));
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$noteOnCommand = F2(
		function (velocity, midiNoteNumber) {
			return _pablobcb$elm_lead$Port$midiOut(
				A2(_pablobcb$elm_lead$Midi$noteOnMessage, midiNoteNumber, velocity));
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$findPressedNote = F2(
		function (model, midiNote) {
			return _elm_lang$core$List$head(
				A2(
					_elm_lang$core$List$filter,
					function (_p0) {
						var _p1 = _p0;
						return _elm_lang$core$Native_Utils.eq(midiNote, _p1._1);
					},
					model.pressedNotes));
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$findPressedKey = F2(
		function (model, symbol) {
			return _elm_lang$core$List$head(
				A2(
					_elm_lang$core$List$filter,
					function (_p2) {
						var _p3 = _p2;
						return _elm_lang$core$Native_Utils.eq(symbol, _p3._0);
					},
					model.pressedNotes));
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$removePressedMidiNote = F2(
		function (model, midiNote) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{
					midiPressedNotes: A2(
						_elm_lang$core$List$filter,
						function (midiNote$) {
							return !_elm_lang$core$Native_Utils.eq(midiNote, midiNote$);
						},
						model.midiPressedNotes)
				});
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$addPressedMidiNote = F2(
		function (model, midiNote) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{
					midiPressedNotes: A2(
						_elm_lang$core$Basics_ops['++'],
						function (_) {
							return _.midiPressedNotes;
						}(model),
						_elm_lang$core$Native_List.fromArray(
							[midiNote]))
				});
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$removePressedNote = F2(
		function (model, symbol) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{
					pressedNotes: A2(
						_elm_lang$core$List$filter,
						function (_p4) {
							var _p5 = _p4;
							return !_elm_lang$core$Native_Utils.eq(symbol, _p5._0);
						},
						model.pressedNotes)
				});
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$removeClickedNote = F2(
		function (model, midiNote) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{
					mousePressedNote: _elm_lang$core$Maybe$Just(midiNote)
				});
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$addClickedNote = F2(
		function (model, midiNote) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{
					mousePressedNote: _elm_lang$core$Maybe$Just(midiNote)
				});
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$mouseLeave = function (model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{mouseHoverNote: _elm_lang$core$Maybe$Nothing, mousePressedNote: _elm_lang$core$Maybe$Nothing});
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$mouseEnter = F2(
		function (model, key) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{
					mouseHoverNote: _elm_lang$core$Maybe$Just(key),
					mousePressedNote: model.clickedAndHovering ? _elm_lang$core$Maybe$Just(key) : _elm_lang$core$Maybe$Nothing
				});
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$mouseUp = function (model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{clickedAndHovering: false, mousePressedNote: _elm_lang$core$Maybe$Nothing});
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$mouseDown = function (model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{clickedAndHovering: true, mousePressedNote: model.mouseHoverNote});
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$octaveUp = function (model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				octave: A2(
					_elm_lang$core$Basics$min,
					8,
					A2(
						F2(
							function (x, y) {
								return x + y;
							}),
						1,
						function (_) {
							return _.octave;
						}(model)))
			});
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$octaveDown = function (model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				octave: A2(
					_elm_lang$core$Basics$max,
					-2,
					function (_) {
						return _.octave;
					}(model) - 1)
			});
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$velocityUp = function (model) {
		var vel = function (_) {
			return _.velocity;
		}(model);
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				velocity: _elm_lang$core$Native_Utils.eq(vel, 1) ? 20 : ((_elm_lang$core$Native_Utils.cmp(vel, 120) > -1) ? 127 : (vel + 20))
			});
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$velocityDown = function (model) {
		var vel = function (_) {
			return _.velocity;
		}(model);
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				velocity: (_elm_lang$core$Native_Utils.cmp(vel, 40) < 0) ? 1 : (_elm_lang$core$Native_Utils.eq(vel, 127) ? 120 : (vel - 20))
			});
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$keyToMidiNoteNumber = function (_p6) {
		var _p7 = _p6;
		var _p11 = _p7._0;
		var _p10 = _p7._1;
		return _pablobcb$elm_lead$Midi$noteToMidiNumber(
			function () {
				var _p8 = _p11;
				switch (_p8.valueOf()) {
					case 'a':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$C, _1: _p10};
					case 'w':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$Db, _1: _p10};
					case 's':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$D, _1: _p10};
					case 'e':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$Eb, _1: _p10};
					case 'd':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$E, _1: _p10};
					case 'f':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$F, _1: _p10};
					case 't':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$Gb, _1: _p10};
					case 'g':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$G, _1: _p10};
					case 'y':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$Ab, _1: _p10};
					case 'h':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$A, _1: _p10};
					case 'u':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$Bb, _1: _p10};
					case 'j':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$B, _1: _p10};
					case 'k':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$C, _1: _p10 + 1};
					case 'o':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$Db, _1: _p10 + 1};
					case 'l':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$D, _1: _p10 + 1};
					case 'p':
						return {ctor: '_Tuple2', _0: _pablobcb$elm_lead$Note$Eb, _1: _p10 + 1};
					default:
						return _elm_lang$core$Native_Utils.crashCase(
							'Container.OnScreenKeyboard.Model',
							{
								start: {line: 97, column: 12},
								end: {line: 147, column: 115}
							},
							_p8)(
							A2(
								_elm_lang$core$Basics_ops['++'],
								'Note and octave outside MIDI bounds: ',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(_p11),
									A2(
										_elm_lang$core$Basics_ops['++'],
										' ',
										_elm_lang$core$Basics$toString(_p10)))));
				}
			}());
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$addPressedNote = F2(
		function (model, symbol) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{
					pressedNotes: A2(
						_elm_lang$core$Basics_ops['++'],
						function (_) {
							return _.pressedNotes;
						}(model),
						_elm_lang$core$Native_List.fromArray(
							[
								{
								ctor: '_Tuple2',
								_0: symbol,
								_1: _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$keyToMidiNoteNumber(
									{
										ctor: '_Tuple2',
										_0: symbol,
										_1: function (_) {
											return _.octave;
										}(model)
									})
							}
							]))
				});
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$unusedKeysOnLastOctave = _elm_lang$core$Native_List.fromArray(
		[
			_elm_lang$core$Native_Utils.chr('h'),
			_elm_lang$core$Native_Utils.chr('u'),
			_elm_lang$core$Native_Utils.chr('j'),
			_elm_lang$core$Native_Utils.chr('k'),
			_elm_lang$core$Native_Utils.chr('o'),
			_elm_lang$core$Native_Utils.chr('l'),
			_elm_lang$core$Native_Utils.chr('p')
		]);
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$pianoKeys = _elm_lang$core$Native_List.fromArray(
		[
			_elm_lang$core$Native_Utils.chr('a'),
			_elm_lang$core$Native_Utils.chr('w'),
			_elm_lang$core$Native_Utils.chr('s'),
			_elm_lang$core$Native_Utils.chr('e'),
			_elm_lang$core$Native_Utils.chr('d'),
			_elm_lang$core$Native_Utils.chr('f'),
			_elm_lang$core$Native_Utils.chr('t'),
			_elm_lang$core$Native_Utils.chr('g'),
			_elm_lang$core$Native_Utils.chr('y'),
			_elm_lang$core$Native_Utils.chr('h'),
			_elm_lang$core$Native_Utils.chr('u'),
			_elm_lang$core$Native_Utils.chr('j'),
			_elm_lang$core$Native_Utils.chr('k'),
			_elm_lang$core$Native_Utils.chr('o'),
			_elm_lang$core$Native_Utils.chr('l'),
			_elm_lang$core$Native_Utils.chr('p')
		]);
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$allowedInputKeys = A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Native_List.fromArray(
			[
				_elm_lang$core$Native_Utils.chr('z'),
				_elm_lang$core$Native_Utils.chr('c'),
				_elm_lang$core$Native_Utils.chr('x'),
				_elm_lang$core$Native_Utils.chr('v')
			]),
		_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$pianoKeys);
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$init = {
		octave: 3,
		velocity: 100,
		pressedNotes: _elm_lang$core$Native_List.fromArray(
			[]),
		clickedAndHovering: false,
		mouseHoverNote: _elm_lang$core$Maybe$Nothing,
		mousePressedNote: _elm_lang$core$Maybe$Nothing,
		midiPressedNotes: _elm_lang$core$Native_List.fromArray(
			[])
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$Model = F7(
		function (a, b, c, d, e, f, g) {
			return {octave: a, velocity: b, pressedNotes: c, clickedAndHovering: d, mouseHoverNote: e, mousePressedNote: f, midiPressedNotes: g};
		});
	
	var _pablobcb$elm_lead$Component_Knob$update = F2(
		function (message, model) {
			var _p0 = message;
			switch (_p0.ctor) {
				case 'NoOp':
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				case 'Reset':
					return (!_elm_lang$core$Native_Utils.eq(model.idKey, _p0._0)) ? {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none} : {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{value: model.initialValue}),
						_1: model.cmdEmitter(model.initialValue)
					};
				case 'MouseDown':
					var _p1 = _p0._1;
					return (!_elm_lang$core$Native_Utils.eq(model.idKey, _p0._0)) ? {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none} : {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{initMouseYPos: _p1, mouseYPos: _p1, isMouseClicked: true}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				case 'MouseUp':
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{isMouseClicked: false}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				default:
					var _p2 = _p0._0;
					var direction = ((model.initMouseYPos - _p2) / _elm_lang$core$Basics$abs(model.initMouseYPos - _p2)) | 0;
					var newValue = model.value + (direction * model.step);
					var model$ = _elm_lang$core$Native_Utils.update(
						model,
						{initMouseYPos: _p2});
					return _elm_lang$core$Basics$not(model$.isMouseClicked) ? {ctor: '_Tuple2', _0: model$, _1: _elm_lang$core$Platform_Cmd$none} : ((_elm_lang$core$Native_Utils.cmp(newValue, model$.max) > 0) ? {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model$,
							{value: model$.max}),
						_1: _elm_lang$core$Platform_Cmd$none
					} : ((_elm_lang$core$Native_Utils.cmp(newValue, model.min) < 0) ? {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model$,
							{value: model.min}),
						_1: _elm_lang$core$Platform_Cmd$none
					} : {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model$,
							{value: newValue}),
						_1: model.cmdEmitter(newValue)
					}));
			}
		});
	var _pablobcb$elm_lead$Component_Knob$adsrLabel = function (knobModel) {
		var knob = function (className) {
			return _elm_lang$core$Native_List.fromArray(
				[
					A2(
					_elm_lang$html$Html$div,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class(
							A2(_elm_lang$core$Basics_ops['++'], 'knob__adsr-label--', className))
						]),
					_elm_lang$core$Native_List.fromArray(
						[]))
				]);
		};
		var _p3 = knobModel.idKey;
		switch (_p3.ctor) {
			case 'AmpAttack':
				return knob('attack');
			case 'FilterAttack':
				return knob('attack');
			case 'AmpDecay':
				return knob('decay');
			case 'FilterDecay':
				return knob('decay');
			case 'AmpSustain':
				return knob('sustain');
			case 'FilterSustain':
				return knob('sustain');
			case 'AmpRelease':
				return knob('release');
			case 'FilterRelease':
				return knob('release');
			default:
				return _elm_lang$core$Native_List.fromArray(
					[]);
		}
	};
	var _pablobcb$elm_lead$Component_Knob$knobDirection = function (model) {
		var valueRange = model.max - model.min;
		var value = _elm_lang$core$Basics$toFloat(model.value) / _elm_lang$core$Basics$toFloat(valueRange);
		var visualRange = 300;
		var visualMaximum = 150;
		var visualMinimum = -150;
		var direction = (visualMinimum + (value * visualRange)) + ((_elm_lang$core$Native_Utils.cmp(model.min, 0) < 0) ? (visualRange / 2) : 0);
		var direction$ = A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Basics$toString(direction),
			'deg');
		return _elm_lang$core$Native_List.fromArray(
			[
				{
				ctor: '_Tuple2',
				_0: 'transform',
				_1: A2(
					_elm_lang$core$Basics_ops['++'],
					'rotate(',
					A2(_elm_lang$core$Basics_ops['++'], direction$, ')'))
			}
			]);
	};
	var _pablobcb$elm_lead$Component_Knob$init = F7(
		function (idKey, value, min, max, step, label, cmdEmitter) {
			return {value: value, initialValue: value, min: min, max: max, step: step * 3, label: label, initMouseYPos: 0, mouseYPos: 0, isMouseClicked: false, cmdEmitter: cmdEmitter, idKey: idKey};
		});
	var _pablobcb$elm_lead$Component_Knob$Model = function (a) {
		return function (b) {
			return function (c) {
				return function (d) {
					return function (e) {
						return function (f) {
							return function (g) {
								return function (h) {
									return function (i) {
										return function (j) {
											return function (k) {
												return {value: a, initialValue: b, min: c, max: d, step: e, label: f, initMouseYPos: g, mouseYPos: h, isMouseClicked: i, cmdEmitter: j, idKey: k};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
	var _pablobcb$elm_lead$Component_Knob$NoOp = {ctor: 'NoOp'};
	var _pablobcb$elm_lead$Component_Knob$Reset = function (a) {
		return {ctor: 'Reset', _0: a};
	};
	var _pablobcb$elm_lead$Component_Knob$MouseUp = {ctor: 'MouseUp'};
	var _pablobcb$elm_lead$Component_Knob$MouseDown = F2(
		function (a, b) {
			return {ctor: 'MouseDown', _0: a, _1: b};
		});
	var _pablobcb$elm_lead$Component_Knob$view = function (model) {
		var adsrLabel$ = _pablobcb$elm_lead$Component_Knob$adsrLabel(model);
		var knobDial = A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					A2(
					_elm_lang$html$Html_Events$on,
					'mousedown',
					A2(
						_elm_lang$core$Json_Decode$map,
						function (posY) {
							return A2(_pablobcb$elm_lead$Component_Knob$MouseDown, model.idKey, posY);
						},
						A2(_elm_lang$core$Json_Decode_ops[':='], 'layerY', _elm_lang$core$Json_Decode$int))),
					A2(
					_elm_lang$html$Html_Events$on,
					'dblclick',
					_elm_lang$core$Json_Decode$succeed(
						_pablobcb$elm_lead$Component_Knob$Reset(model.idKey))),
					A3(
					_elm_lang$html$Html_Events$onWithOptions,
					'dragstart',
					{stopPropagation: true, preventDefault: true},
					_elm_lang$core$Json_Decode$succeed(_pablobcb$elm_lead$Component_Knob$NoOp)),
					_elm_lang$html$Html_Attributes$class('knob__dial'),
					A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', 'false'),
					_elm_lang$html$Html_Attributes$style(
					_pablobcb$elm_lead$Component_Knob$knobDirection(model))
				]),
			_elm_lang$core$Native_List.fromArray(
				[]));
		var knobLabelHtml = A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$class('knob__label')
				]),
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html$text(model.label)
				]));
		var knob = A2(
			_elm_lang$core$Basics_ops['++'],
			adsrLabel$,
			_elm_lang$core$Native_List.fromArray(
				[
					A2(
					_elm_lang$html$Html$div,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('knob__scale')
						]),
					_elm_lang$core$Native_List.fromArray(
						[knobDial])),
					knobLabelHtml
				]));
		return A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$class('knob')
				]),
			knob);
	};
	var _pablobcb$elm_lead$Component_Knob$knob = F2(
		function (knobMsg, model) {
			return A2(
				_elm_lang$html$Html_App$map,
				knobMsg,
				_pablobcb$elm_lead$Component_Knob$view(model));
		});
	var _pablobcb$elm_lead$Component_Knob$MouseMove = function (a) {
		return {ctor: 'MouseMove', _0: a};
	};
	var _pablobcb$elm_lead$Component_Knob$Release = {ctor: 'Release'};
	var _pablobcb$elm_lead$Component_Knob$Sustain = {ctor: 'Sustain'};
	var _pablobcb$elm_lead$Component_Knob$Decay = {ctor: 'Decay'};
	var _pablobcb$elm_lead$Component_Knob$Attack = {ctor: 'Attack'};
	var _pablobcb$elm_lead$Component_Knob$FilterEnvelopeAmount = {ctor: 'FilterEnvelopeAmount'};
	var _pablobcb$elm_lead$Component_Knob$FilterRelease = {ctor: 'FilterRelease'};
	var _pablobcb$elm_lead$Component_Knob$FilterSustain = {ctor: 'FilterSustain'};
	var _pablobcb$elm_lead$Component_Knob$FilterDecay = {ctor: 'FilterDecay'};
	var _pablobcb$elm_lead$Component_Knob$FilterAttack = {ctor: 'FilterAttack'};
	var _pablobcb$elm_lead$Component_Knob$FilterQ = {ctor: 'FilterQ'};
	var _pablobcb$elm_lead$Component_Knob$FilterCutoff = {ctor: 'FilterCutoff'};
	var _pablobcb$elm_lead$Component_Knob$AmpRelease = {ctor: 'AmpRelease'};
	var _pablobcb$elm_lead$Component_Knob$AmpSustain = {ctor: 'AmpSustain'};
	var _pablobcb$elm_lead$Component_Knob$AmpDecay = {ctor: 'AmpDecay'};
	var _pablobcb$elm_lead$Component_Knob$AmpAttack = {ctor: 'AmpAttack'};
	var _pablobcb$elm_lead$Component_Knob$AmpGain = {ctor: 'AmpGain'};
	var _pablobcb$elm_lead$Component_Knob$FM = {ctor: 'FM'};
	var _pablobcb$elm_lead$Component_Knob$Osc2Detune = {ctor: 'Osc2Detune'};
	var _pablobcb$elm_lead$Component_Knob$Osc2Semitone = {ctor: 'Osc2Semitone'};
	var _pablobcb$elm_lead$Component_Knob$PW = {ctor: 'PW'};
	var _pablobcb$elm_lead$Component_Knob$OscMix = {ctor: 'OscMix'};
	
	var _pablobcb$elm_lead$Component_Switch$update = F2(
		function (message, model) {
			var _p0 = message;
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_Utils.update(
					model,
					{
						on: _elm_lang$core$Basics$not(model.on)
					}),
				_1: model.cmdEmitter(
					_elm_lang$core$Basics$not(model.on))
			};
		});
	var _pablobcb$elm_lead$Component_Switch$init = F2(
		function (on, cmdEmitter) {
			return {on: on, cmdEmitter: cmdEmitter};
		});
	var _pablobcb$elm_lead$Component_Switch$Model = F2(
		function (a, b) {
			return {on: a, cmdEmitter: b};
		});
	var _pablobcb$elm_lead$Component_Switch$Click = {ctor: 'Click'};
	var _pablobcb$elm_lead$Component_Switch$view = F2(
		function (label, model) {
			var state = model.on ? 'switch__item--active' : 'switch__item--inactive';
			return A2(
				_elm_lang$html$Html$div,
				_elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$html$Html_Attributes$class('switch')
					]),
				_elm_lang$core$Native_List.fromArray(
					[
						A2(
						_elm_lang$html$Html$div,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html_Attributes$class('switch__item')
							]),
						_elm_lang$core$Native_List.fromArray(
							[
								A2(
								_elm_lang$html$Html$div,
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html_Attributes$class(state)
									]),
								_elm_lang$core$Native_List.fromArray(
									[])),
								A2(
								_elm_lang$html$Html$div,
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html_Attributes$class('switch__label')
									]),
								_elm_lang$core$Native_List.fromArray(
									[
										_elm_lang$html$Html$text(label)
									]))
							])),
						A2(
						_elm_lang$html$Html$button,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html_Attributes$class('switch__btn'),
								_elm_lang$html$Html_Events$onClick(_pablobcb$elm_lead$Component_Switch$Click)
							]),
						_elm_lang$core$Native_List.fromArray(
							[]))
					]));
		});
	var _pablobcb$elm_lead$Component_Switch$switch = F3(
		function (label, switchMsg, model) {
			return A2(
				_elm_lang$html$Html_App$map,
				switchMsg,
				A2(_pablobcb$elm_lead$Component_Switch$view, label, model));
		});
	
	var _pablobcb$elm_lead$Component_OptionPicker$getNextElem = function (elems) {
		var nextElem = function () {
			var _p0 = _elm_lang$core$List$head(elems);
			if (_p0.ctor === 'Just') {
				return _p0._0;
			} else {
				return _elm_lang$core$Native_Utils.crashCase(
					'Component.OptionPicker',
					{
						start: {line: 99, column: 13},
						end: {line: 104, column: 53}
					},
					_p0)('no values provided');
			}
		}();
		return nextElem;
	};
	var _pablobcb$elm_lead$Component_OptionPicker$update = F2(
		function (message, model) {
			var _p2 = message;
			var elems = _elm_community$elm_lazy_list$Lazy_List$cycle(
				_elm_community$elm_lazy_list$Lazy_List$fromList(model.elems));
			var elems$ = _elm_community$elm_lazy_list$Lazy_List$toList(
				A2(
					_elm_community$elm_lazy_list$Lazy_List$take,
					_elm_lang$core$List$length(model.elems),
					A2(_elm_community$elm_lazy_list$Lazy_List$drop, 1, elems)));
			var nextElem = _elm_lang$core$Basics$snd(
				_pablobcb$elm_lead$Component_OptionPicker$getNextElem(elems$));
			var model$ = _elm_lang$core$Native_Utils.update(
				model,
				{elems: elems$, currentElem: nextElem});
			return {
				ctor: '_Tuple2',
				_0: model$,
				_1: model.cmdEmmiter(
					_elm_lang$core$Basics$toString(nextElem))
			};
		});
	var _pablobcb$elm_lead$Component_OptionPicker$option = F3(
		function (model, elem, name) {
			var state = _elm_lang$core$Native_Utils.eq(elem, model.currentElem) ? 'option-picker__item--active' : 'option-picker__item--inactive';
			return A2(
				_elm_lang$html$Html$li,
				_elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$html$Html_Attributes$class('option-picker__item')
					]),
				_elm_lang$core$Native_List.fromArray(
					[
						A2(
						_elm_lang$html$Html$div,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html_Attributes$class(state)
							]),
						_elm_lang$core$Native_List.fromArray(
							[])),
						A2(
						_elm_lang$html$Html$div,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html_Attributes$class(
								A2(_elm_lang$core$Basics_ops['++'], 'option-picker__id--', name))
							]),
						_elm_lang$core$Native_List.fromArray(
							[]))
					]));
		});
	var _pablobcb$elm_lead$Component_OptionPicker$options = function (model) {
		return A2(
			_elm_lang$core$List$map,
			function (_p3) {
				var _p4 = _p3;
				return A3(_pablobcb$elm_lead$Component_OptionPicker$option, model, _p4._1, _p4._0);
			},
			model.options);
	};
	var _pablobcb$elm_lead$Component_OptionPicker$init = F3(
		function (cmdEmmiter, selected, elems) {
			var orderedElems = _elm_community$elm_lazy_list$Lazy_List$toList(
				A2(
					_elm_community$elm_lazy_list$Lazy_List$take,
					_elm_lang$core$List$length(elems),
					A2(
						_elm_community$elm_lazy_list$Lazy_List$dropWhile,
						function (_p5) {
							var _p6 = _p5;
							return !_elm_lang$core$Native_Utils.eq(_p6._1, selected);
						},
						_elm_community$elm_lazy_list$Lazy_List$cycle(
							_elm_community$elm_lazy_list$Lazy_List$fromList(elems)))));
			var selectedElem = _elm_lang$core$Basics$snd(
				function () {
					var _p7 = _elm_lang$core$List$head(orderedElems);
					if (_p7.ctor === 'Just') {
						return _p7._0;
					} else {
						return _elm_lang$core$Native_Utils.crashCase(
							'Component.OptionPicker',
							{
								start: {line: 38, column: 20},
								end: {line: 43, column: 69}
							},
							_p7)('empty list on button creation!');
					}
				}());
			return {currentElem: selectedElem, options: elems, cmdEmmiter: cmdEmmiter, elems: orderedElems};
		});
	var _pablobcb$elm_lead$Component_OptionPicker$Model = F4(
		function (a, b, c, d) {
			return {elems: a, currentElem: b, options: c, cmdEmmiter: d};
		});
	var _pablobcb$elm_lead$Component_OptionPicker$Click = {ctor: 'Click'};
	var _pablobcb$elm_lead$Component_OptionPicker$view = F2(
		function (label, model) {
			return A2(
				_elm_lang$html$Html$div,
				_elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$html$Html_Attributes$class('option-picker')
					]),
				_elm_lang$core$Native_List.fromArray(
					[
						A2(
						_elm_lang$html$Html$span,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html_Attributes$class('option-picker__label')
							]),
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html$text(label)
							])),
						A2(
						_elm_lang$html$Html$ul,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html_Attributes$class('option-picker__list')
							]),
						_pablobcb$elm_lead$Component_OptionPicker$options(model)),
						A2(
						_elm_lang$html$Html$button,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html_Attributes$class('option-picker__btn'),
								_elm_lang$html$Html_Events$onClick(_pablobcb$elm_lead$Component_OptionPicker$Click)
							]),
						_elm_lang$core$Native_List.fromArray(
							[]))
					]));
		});
	var _pablobcb$elm_lead$Component_OptionPicker$optionPicker = F3(
		function (label, pickerMsg, model) {
			return A2(
				_elm_lang$html$Html_App$map,
				pickerMsg,
				A2(_pablobcb$elm_lead$Component_OptionPicker$view, label, model));
		});
	
	var _pablobcb$elm_lead$Container_Panel_Model$updateOverdriveSwitch = F2(
		function ($switch, model) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{overdriveSwitch: $switch});
		});
	var _pablobcb$elm_lead$Container_Panel_Model$updateFilterTypeBtn = F2(
		function (btn, model) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{filterTypeBtn: btn});
		});
	var _pablobcb$elm_lead$Container_Panel_Model$updateOsc2KbdTrack = F2(
		function ($switch, model) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{osc2KbdTrackSwitch: $switch});
		});
	var _pablobcb$elm_lead$Container_Panel_Model$updateOsc2WaveformBtn = F2(
		function (btn, model) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{osc2WaveformBtn: btn});
		});
	var _pablobcb$elm_lead$Container_Panel_Model$updateOsc1WaveformBtn = F2(
		function (btn, model) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{osc1WaveformBtn: btn});
		});
	var _pablobcb$elm_lead$Container_Panel_Model$findKnob = F2(
		function (model, knobInstance) {
			var knob = _elm_lang$core$List$head(
				A2(
					_elm_lang$core$List$filter,
					function (knob$) {
						return _elm_lang$core$Native_Utils.eq(knob$.idKey, knobInstance);
					},
					model.knobs));
			var _p0 = knob;
			if (_p0.ctor === 'Nothing') {
				return _elm_lang$core$Native_Utils.crashCase(
					'Container.Panel.Model',
					{
						start: {line: 256, column: 9},
						end: {line: 261, column: 26}
					},
					_p0)('inexistent knob identifier');
			} else {
				return _p0._0;
			}
		});
	var _pablobcb$elm_lead$Container_Panel_Model$knobs = function (preset) {
		return _elm_lang$core$Native_List.fromArray(
			[
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$OscMix, preset.oscs.mix, 0, 127, 1, 'OscMix', _pablobcb$elm_lead$Port$oscsBalance),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$PW, preset.oscs.pw, 0, 127, 1, 'PW', _pablobcb$elm_lead$Port$pulseWidth),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$Osc2Semitone, preset.oscs.osc2.semitone, -60, 60, 1, 'semitone', _pablobcb$elm_lead$Port$osc2Semitone),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$Osc2Detune, preset.oscs.osc2.detune, -100, 100, 1, 'detune', _pablobcb$elm_lead$Port$osc2Detune),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$FM, preset.oscs.osc1.fmGain, 0, 127, 1, 'FM', _pablobcb$elm_lead$Port$fmAmount),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$AmpGain, preset.amp.masterVolume, 0, 127, 1, 'gain', _pablobcb$elm_lead$Port$ampVolume),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$AmpAttack, preset.amp.adsr.attack, 0, 127, 1, 'attack', _pablobcb$elm_lead$Port$ampAttack),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$AmpDecay, preset.amp.adsr.decay, 0, 127, 1, 'decay', _pablobcb$elm_lead$Port$ampDecay),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$AmpSustain, preset.amp.adsr.sustain, 0, 127, 1, 'sustain', _pablobcb$elm_lead$Port$ampSustain),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$AmpRelease, preset.amp.adsr.release, 0, 127, 1, 'release', _pablobcb$elm_lead$Port$ampRelease),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$FilterCutoff, preset.filter.frequency, 0, 127, 1, 'frequency', _pablobcb$elm_lead$Port$filterCutoff),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$FilterQ, preset.filter.q, 0, 127, 1, 'resonance', _pablobcb$elm_lead$Port$filterQ),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$FilterAttack, preset.filter.adsr.attack, 0, 127, 1, 'attack', _pablobcb$elm_lead$Port$filterAttack),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$FilterDecay, preset.filter.adsr.decay, 0, 127, 1, 'decay', _pablobcb$elm_lead$Port$filterDecay),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$FilterSustain, preset.filter.adsr.sustain, 0, 127, 1, 'sustain', _pablobcb$elm_lead$Port$filterSustain),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$FilterRelease, preset.filter.adsr.release, 0, 127, 1, 'release', _pablobcb$elm_lead$Port$filterRelease),
				A7(_pablobcb$elm_lead$Component_Knob$init, _pablobcb$elm_lead$Component_Knob$FilterEnvelopeAmount, preset.filter.envelopeAmount, 0, 127, 1, 'env amnt', _pablobcb$elm_lead$Port$filterEnvelopeAmount)
			]);
	};
	var _pablobcb$elm_lead$Container_Panel_Model$Model = F6(
		function (a, b, c, d, e, f) {
			return {knobs: a, filterTypeBtn: b, osc2WaveformBtn: c, osc1WaveformBtn: d, osc2KbdTrackSwitch: e, overdriveSwitch: f};
		});
	var _pablobcb$elm_lead$Container_Panel_Model$WhiteNoise = {ctor: 'WhiteNoise'};
	var _pablobcb$elm_lead$Container_Panel_Model$Pulse = {ctor: 'Pulse'};
	var _pablobcb$elm_lead$Container_Panel_Model$Square = {ctor: 'Square'};
	var _pablobcb$elm_lead$Container_Panel_Model$Sine = {ctor: 'Sine'};
	var _pablobcb$elm_lead$Container_Panel_Model$Triangle = {ctor: 'Triangle'};
	var _pablobcb$elm_lead$Container_Panel_Model$Sawtooth = {ctor: 'Sawtooth'};
	var _pablobcb$elm_lead$Container_Panel_Model$createOscillatorWaveform = function (name) {
		var _p2 = name;
		switch (_p2) {
			case 'sawtooth':
				return _pablobcb$elm_lead$Container_Panel_Model$Sawtooth;
			case 'triangle':
				return _pablobcb$elm_lead$Container_Panel_Model$Triangle;
			case 'sine':
				return _pablobcb$elm_lead$Container_Panel_Model$Sine;
			case 'square':
				return _pablobcb$elm_lead$Container_Panel_Model$Square;
			case 'whitenoise':
				return _pablobcb$elm_lead$Container_Panel_Model$WhiteNoise;
			default:
				return _elm_lang$core$Native_Utils.crashCase(
					'Container.Panel.Model',
					{
						start: {line: 34, column: 5},
						end: {line: 51, column: 66}
					},
					_p2)(
					A2(
						_elm_lang$core$Basics_ops['++'],
						'invalid waveform ',
						_elm_lang$core$Basics$toString(name)));
		}
	};
	var _pablobcb$elm_lead$Container_Panel_Model$Notch = {ctor: 'Notch'};
	var _pablobcb$elm_lead$Container_Panel_Model$Bandpass = {ctor: 'Bandpass'};
	var _pablobcb$elm_lead$Container_Panel_Model$Highpass = {ctor: 'Highpass'};
	var _pablobcb$elm_lead$Container_Panel_Model$Lowpass = {ctor: 'Lowpass'};
	var _pablobcb$elm_lead$Container_Panel_Model$createFilterType = function (name) {
		var _p4 = name;
		switch (_p4) {
			case 'lowpass':
				return _pablobcb$elm_lead$Container_Panel_Model$Lowpass;
			case 'highpass':
				return _pablobcb$elm_lead$Container_Panel_Model$Highpass;
			case 'bandpass':
				return _pablobcb$elm_lead$Container_Panel_Model$Bandpass;
			case 'notch':
				return _pablobcb$elm_lead$Container_Panel_Model$Notch;
			default:
				return _elm_lang$core$Native_Utils.crashCase(
					'Container.Panel.Model',
					{
						start: {line: 63, column: 5},
						end: {line: 77, column: 69}
					},
					_p4)(
					A2(
						_elm_lang$core$Basics_ops['++'],
						'invalid filter type ',
						_elm_lang$core$Basics$toString(name)));
		}
	};
	var _pablobcb$elm_lead$Container_Panel_Model$init = function (preset) {
		return {
			knobs: _pablobcb$elm_lead$Container_Panel_Model$knobs(preset),
			osc2KbdTrackSwitch: A2(_pablobcb$elm_lead$Component_Switch$init, preset.oscs.osc2.kbdTrack, _pablobcb$elm_lead$Port$osc2KbdTrack),
			overdriveSwitch: A2(_pablobcb$elm_lead$Component_Switch$init, preset.overdrive, _pablobcb$elm_lead$Port$overdrive),
			filterTypeBtn: A3(
				_pablobcb$elm_lead$Component_OptionPicker$init,
				_pablobcb$elm_lead$Port$filterType,
				_pablobcb$elm_lead$Container_Panel_Model$createFilterType(preset.filter.type_),
				_elm_lang$core$Native_List.fromArray(
					[
						{ctor: '_Tuple2', _0: 'LP', _1: _pablobcb$elm_lead$Container_Panel_Model$Lowpass},
						{ctor: '_Tuple2', _0: 'HP', _1: _pablobcb$elm_lead$Container_Panel_Model$Highpass},
						{ctor: '_Tuple2', _0: 'BP', _1: _pablobcb$elm_lead$Container_Panel_Model$Bandpass},
						{ctor: '_Tuple2', _0: 'notch', _1: _pablobcb$elm_lead$Container_Panel_Model$Notch}
					])),
			osc1WaveformBtn: A3(
				_pablobcb$elm_lead$Component_OptionPicker$init,
				_pablobcb$elm_lead$Port$osc1Waveform,
				_pablobcb$elm_lead$Container_Panel_Model$createOscillatorWaveform(preset.oscs.osc1.waveformType),
				_elm_lang$core$Native_List.fromArray(
					[
						{ctor: '_Tuple2', _0: 'sin', _1: _pablobcb$elm_lead$Container_Panel_Model$Sine},
						{ctor: '_Tuple2', _0: 'tri', _1: _pablobcb$elm_lead$Container_Panel_Model$Triangle},
						{ctor: '_Tuple2', _0: 'saw', _1: _pablobcb$elm_lead$Container_Panel_Model$Sawtooth},
						{ctor: '_Tuple2', _0: 'sqr', _1: _pablobcb$elm_lead$Container_Panel_Model$Square}
					])),
			osc2WaveformBtn: A3(
				_pablobcb$elm_lead$Component_OptionPicker$init,
				_pablobcb$elm_lead$Port$osc2Waveform,
				_pablobcb$elm_lead$Container_Panel_Model$createOscillatorWaveform(preset.oscs.osc2.waveformType),
				_elm_lang$core$Native_List.fromArray(
					[
						{ctor: '_Tuple2', _0: 'tri', _1: _pablobcb$elm_lead$Container_Panel_Model$Triangle},
						{ctor: '_Tuple2', _0: 'saw', _1: _pablobcb$elm_lead$Container_Panel_Model$Sawtooth},
						{ctor: '_Tuple2', _0: 'pulse', _1: _pablobcb$elm_lead$Container_Panel_Model$Pulse},
						{ctor: '_Tuple2', _0: 'noise', _1: _pablobcb$elm_lead$Container_Panel_Model$WhiteNoise}
					]))
		};
	};
	
	var _pablobcb$elm_lead$Main_Model$updatePanel = F2(
		function (panel, model) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{panel: panel});
		});
	var _pablobcb$elm_lead$Main_Model$updateOnScreenKeyboard = F2(
		function (keyboard, model) {
			return _elm_lang$core$Native_Utils.update(
				model,
				{onScreenKeyboard: keyboard});
		});
	var _pablobcb$elm_lead$Main_Model$init = F2(
		function (preset, midiSupport) {
			return {
				onScreenKeyboard: _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$init,
				panel: _pablobcb$elm_lead$Container_Panel_Model$init(preset),
				midiConnected: false,
				midiMsgInLedOn: false,
				midiSupport: midiSupport,
				presetName: preset.name,
				presetId: preset.presetId
			};
		});
	var _pablobcb$elm_lead$Main_Model$InitialFlags = F2(
		function (a, b) {
			return {preset: a, midiSupport: b};
		});
	var _pablobcb$elm_lead$Main_Model$Model = F7(
		function (a, b, c, d, e, f, g) {
			return {onScreenKeyboard: a, panel: b, midiConnected: c, midiMsgInLedOn: d, midiSupport: e, presetName: f, presetId: g};
		});
	
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$update = F2(
		function (msg, model) {
			var _p0 = msg;
			switch (_p0.ctor) {
				case 'Panic':
					return {
						ctor: '_Tuple2',
						_0: _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$panic(model),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				case 'MidiMessageIn':
					var _p1 = _p0._0;
					_v1_2:
					do {
						if ((((_p1.ctor === '::') && (_p1._0.ctor === 'Just')) && (_p1._1.ctor === '::')) && (_p1._1._0.ctor === 'Just')) {
							switch (_p1._0._0) {
								case 144:
									return {
										ctor: '_Tuple2',
										_0: A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$addPressedMidiNote, model, _p1._1._0._0),
										_1: _elm_lang$core$Platform_Cmd$none
									};
								case 128:
									return {
										ctor: '_Tuple2',
										_0: A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$removePressedMidiNote, model, _p1._1._0._0),
										_1: _elm_lang$core$Platform_Cmd$none
									};
								default:
									break _v1_2;
							}
						} else {
							break _v1_2;
						}
					} while(false);
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				case 'NoOp':
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				case 'MouseDown':
					var model$ = _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$mouseDown(model);
					var isKeyPressed = function (midiNoteNumber) {
						return _elm_community$maybe_extra$Maybe_Extra$isJust(
							A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$findPressedNote, model$, midiNoteNumber));
					};
					var hoveringAndClickingKey = model$.mousePressedNote;
					var _p2 = hoveringAndClickingKey;
					if (_p2.ctor === 'Just') {
						var _p3 = _p2._0;
						return isKeyPressed(_p3) ? {ctor: '_Tuple2', _0: model$, _1: _elm_lang$core$Platform_Cmd$none} : {
							ctor: '_Tuple2',
							_0: model$,
							_1: A2(
								_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$noteOnCommand,
								function (_) {
									return _.velocity;
								}(model$),
								_p3)
						};
					} else {
						return {ctor: '_Tuple2', _0: model$, _1: _elm_lang$core$Platform_Cmd$none};
					}
				case 'MouseUp':
					var model$ = _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$mouseUp(model);
					var hoveringAndClickingKey = model.mousePressedNote;
					var isKeyPressed = function (midiNoteNumber) {
						return _elm_community$maybe_extra$Maybe_Extra$isJust(
							A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$findPressedNote, model, midiNoteNumber));
					};
					var _p4 = hoveringAndClickingKey;
					if (_p4.ctor === 'Just') {
						var _p5 = _p4._0;
						return isKeyPressed(_p5) ? {ctor: '_Tuple2', _0: model$, _1: _elm_lang$core$Platform_Cmd$none} : {
							ctor: '_Tuple2',
							_0: model$,
							_1: A2(
								_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$noteOffCommand,
								function (_) {
									return _.velocity;
								}(model$),
								_p5)
						};
					} else {
						return {ctor: '_Tuple2', _0: model$, _1: _elm_lang$core$Platform_Cmd$none};
					}
				case 'MouseEnter':
					var model$ = A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$mouseEnter, model, _p0._0);
					var isKeyPressed = function (midiNoteNumber) {
						return _elm_community$maybe_extra$Maybe_Extra$isJust(
							A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$findPressedNote, model$, midiNoteNumber));
					};
					var hoveringAndClickingKey = model$.mousePressedNote;
					var _p6 = hoveringAndClickingKey;
					if (_p6.ctor === 'Just') {
						var _p7 = _p6._0;
						return isKeyPressed(_p7) ? {ctor: '_Tuple2', _0: model$, _1: _elm_lang$core$Platform_Cmd$none} : {
							ctor: '_Tuple2',
							_0: model$,
							_1: A2(
								_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$noteOnCommand,
								function (_) {
									return _.velocity;
								}(model$),
								_p7)
						};
					} else {
						return {ctor: '_Tuple2', _0: model$, _1: _elm_lang$core$Platform_Cmd$none};
					}
				case 'MouseLeave':
					var model$ = _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$mouseLeave(model);
					var hoveringAndClickingKey = model.mousePressedNote;
					var isKeyPressed = function (midiNoteNumber) {
						return _elm_community$maybe_extra$Maybe_Extra$isJust(
							A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$findPressedNote, model, midiNoteNumber));
					};
					var _p8 = hoveringAndClickingKey;
					if (_p8.ctor === 'Just') {
						var _p9 = _p8._0;
						return isKeyPressed(_p9) ? {ctor: '_Tuple2', _0: model$, _1: _elm_lang$core$Platform_Cmd$none} : {
							ctor: '_Tuple2',
							_0: model$,
							_1: A2(
								_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$noteOffCommand,
								function (_) {
									return _.velocity;
								}(model$),
								_p9)
						};
					} else {
						return {ctor: '_Tuple2', _0: model$, _1: _elm_lang$core$Platform_Cmd$none};
					}
				case 'OctaveDown':
					return {
						ctor: '_Tuple2',
						_0: _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$octaveDown(model),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				case 'OctaveUp':
					return {
						ctor: '_Tuple2',
						_0: _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$octaveUp(model),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				case 'VelocityDown':
					return {
						ctor: '_Tuple2',
						_0: _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$velocityDown(model),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				case 'VelocityUp':
					return {
						ctor: '_Tuple2',
						_0: _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$velocityUp(model),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				case 'KeyOn':
					var _p11 = _p0._0;
					var hoveringAndClickingKey = model.mousePressedNote;
					var midiNoteNumber = _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$keyToMidiNoteNumber(
						{ctor: '_Tuple2', _0: _p11, _1: model.octave});
					var model$ = A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$addPressedNote, model, _p11);
					var _p10 = hoveringAndClickingKey;
					if (_p10.ctor === 'Just') {
						return _elm_lang$core$Native_Utils.eq(midiNoteNumber, _p10._0) ? {ctor: '_Tuple2', _0: model$, _1: _elm_lang$core$Platform_Cmd$none} : {
							ctor: '_Tuple2',
							_0: model$,
							_1: A2(
								_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$noteOnCommand,
								function (_) {
									return _.velocity;
								}(model),
								midiNoteNumber)
						};
					} else {
						return {
							ctor: '_Tuple2',
							_0: model$,
							_1: A2(
								_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$noteOnCommand,
								function (_) {
									return _.velocity;
								}(model),
								midiNoteNumber)
						};
					}
				default:
					var _p15 = _p0._0;
					var model$ = A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$removePressedNote, model, _p15);
					var hoveringAndClickingKey = model.mousePressedNote;
					var releasedKey = A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$findPressedKey, model, _p15);
					var _p12 = releasedKey;
					if (_p12.ctor === 'Just') {
						var _p14 = _p12._0._1;
						var _p13 = hoveringAndClickingKey;
						if (_p13.ctor === 'Just') {
							return _elm_lang$core$Native_Utils.eq(_p14, _p13._0) ? {ctor: '_Tuple2', _0: model$, _1: _elm_lang$core$Platform_Cmd$none} : {
								ctor: '_Tuple2',
								_0: model$,
								_1: A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$noteOffCommand, model.velocity, _p14)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: model$,
								_1: A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$noteOffCommand, model.velocity, _p14)
							};
						}
					} else {
						return {ctor: '_Tuple2', _0: model$, _1: _elm_lang$core$Platform_Cmd$none};
					}
			}
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$Panic = {ctor: 'Panic'};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$NoOp = {ctor: 'NoOp'};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$MidiMessageIn = function (a) {
		return {ctor: 'MidiMessageIn', _0: a};
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$MouseDown = {ctor: 'MouseDown'};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$MouseUp = {ctor: 'MouseUp'};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$KeyOff = function (a) {
		return {ctor: 'KeyOff', _0: a};
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$handleKeyUp = F2(
		function (msg, keyCode) {
			var symbol = _elm_lang$core$Char$toLower(
				_elm_lang$core$Char$fromCode(keyCode));
			var invalidKey = _elm_lang$core$Basics$not(
				A2(_elm_lang$core$List$member, symbol, _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$pianoKeys));
			return invalidKey ? msg(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$NoOp) : msg(
				_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$KeyOff(symbol));
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$KeyOn = function (a) {
		return {ctor: 'KeyOn', _0: a};
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$MouseLeave = function (a) {
		return {ctor: 'MouseLeave', _0: a};
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$MouseEnter = function (a) {
		return {ctor: 'MouseEnter', _0: a};
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$VelocityDown = {ctor: 'VelocityDown'};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$VelocityUp = {ctor: 'VelocityUp'};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$OctaveDown = {ctor: 'OctaveDown'};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$OctaveUp = {ctor: 'OctaveUp'};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$handleKeyDown = F3(
		function (msg, model, keyCode) {
			var isLastOctave = _elm_lang$core$Native_Utils.eq(
				function (_) {
					return _.octave;
				}(model),
				8);
			var symbol = _elm_lang$core$Char$toLower(
				_elm_lang$core$Char$fromCode(keyCode));
			var allowedInput = A2(_elm_lang$core$List$member, symbol, _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$allowedInputKeys);
			var unusedKeys = A2(_elm_lang$core$List$member, symbol, _pablobcb$elm_lead$Container_OnScreenKeyboard_Model$unusedKeysOnLastOctave);
			var symbolAlreadyPressed = _elm_community$maybe_extra$Maybe_Extra$isJust(
				A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Model$findPressedKey, model, symbol));
			if (_elm_lang$core$Basics$not(allowedInput) || ((isLastOctave && unusedKeys) || symbolAlreadyPressed)) {
				return msg(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$NoOp);
			} else {
				var _p16 = symbol;
				switch (_p16.valueOf()) {
					case 'z':
						return msg(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$OctaveDown);
					case 'x':
						return msg(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$OctaveUp);
					case 'c':
						return msg(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$VelocityDown);
					case 'v':
						return msg(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$VelocityUp);
					default:
						return msg(
							_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$KeyOn(_p16));
				}
			}
		});
	
	var _pablobcb$elm_lead$Container_Panel_Update$KnobMsg = function (a) {
		return {ctor: 'KnobMsg', _0: a};
	};
	var _pablobcb$elm_lead$Container_Panel_Update$FilterTypeChange = function (a) {
		return {ctor: 'FilterTypeChange', _0: a};
	};
	var _pablobcb$elm_lead$Container_Panel_Update$OverdriveToggle = function (a) {
		return {ctor: 'OverdriveToggle', _0: a};
	};
	var _pablobcb$elm_lead$Container_Panel_Update$Osc2KbdTrackToggle = function (a) {
		return {ctor: 'Osc2KbdTrackToggle', _0: a};
	};
	var _pablobcb$elm_lead$Container_Panel_Update$Osc2WaveformChange = function (a) {
		return {ctor: 'Osc2WaveformChange', _0: a};
	};
	var _pablobcb$elm_lead$Container_Panel_Update$Osc1WaveformChange = function (a) {
		return {ctor: 'Osc1WaveformChange', _0: a};
	};
	var _pablobcb$elm_lead$Container_Panel_Update$update = F2(
		function (msg, model) {
			var updateKnobs = F2(
				function (knobMsg, msg$) {
					var _p0 = _elm_lang$core$List$unzip(
						A2(
							_elm_lang$core$List$map,
							function (knob) {
								return A2(_pablobcb$elm_lead$Component_Knob$update, knobMsg, knob);
							},
							model.knobs));
					var knobs = _p0._0;
					var cmds = _p0._1;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{knobs: knobs}),
						_1: A2(
							_elm_lang$core$Platform_Cmd$map,
							_elm_lang$core$Basics$always(msg$),
							_elm_lang$core$Platform_Cmd$batch(cmds))
					};
				});
			var updateMap = F5(
				function (childUpdate, childMsg, getChild, reduxor, msg$) {
					var _p1 = A2(
						childUpdate,
						childMsg,
						getChild(model));
					var updatedChildModel = _p1._0;
					var childCmd = _p1._1;
					return {
						ctor: '_Tuple2',
						_0: A2(reduxor, updatedChildModel, model),
						_1: A2(_elm_lang$core$Platform_Cmd$map, msg$, childCmd)
					};
				});
			var _p2 = msg;
			switch (_p2.ctor) {
				case 'Osc1WaveformChange':
					return A5(
						updateMap,
						_pablobcb$elm_lead$Component_OptionPicker$update,
						_p2._0,
						function (_) {
							return _.osc1WaveformBtn;
						},
						_pablobcb$elm_lead$Container_Panel_Model$updateOsc1WaveformBtn,
						_pablobcb$elm_lead$Container_Panel_Update$Osc1WaveformChange);
				case 'Osc2WaveformChange':
					return A5(
						updateMap,
						_pablobcb$elm_lead$Component_OptionPicker$update,
						_p2._0,
						function (_) {
							return _.osc2WaveformBtn;
						},
						_pablobcb$elm_lead$Container_Panel_Model$updateOsc2WaveformBtn,
						_pablobcb$elm_lead$Container_Panel_Update$Osc2WaveformChange);
				case 'FilterTypeChange':
					return A5(
						updateMap,
						_pablobcb$elm_lead$Component_OptionPicker$update,
						_p2._0,
						function (_) {
							return _.filterTypeBtn;
						},
						_pablobcb$elm_lead$Container_Panel_Model$updateFilterTypeBtn,
						_pablobcb$elm_lead$Container_Panel_Update$FilterTypeChange);
				case 'Osc2KbdTrackToggle':
					return A5(
						updateMap,
						_pablobcb$elm_lead$Component_Switch$update,
						_p2._0,
						function (_) {
							return _.osc2KbdTrackSwitch;
						},
						_pablobcb$elm_lead$Container_Panel_Model$updateOsc2KbdTrack,
						_pablobcb$elm_lead$Container_Panel_Update$Osc2KbdTrackToggle);
				case 'OverdriveToggle':
					return A5(
						updateMap,
						_pablobcb$elm_lead$Component_Switch$update,
						_p2._0,
						function (_) {
							return _.overdriveSwitch;
						},
						_pablobcb$elm_lead$Container_Panel_Model$updateOverdriveSwitch,
						_pablobcb$elm_lead$Container_Panel_Update$OverdriveToggle);
				default:
					var _p3 = _p2._0;
					return A2(
						updateKnobs,
						_p3,
						_pablobcb$elm_lead$Container_Panel_Update$KnobMsg(_p3));
			}
		});
	
	var _pablobcb$elm_lead$Main_Update$PresetChange = function (a) {
		return {ctor: 'PresetChange', _0: a};
	};
	var _pablobcb$elm_lead$Main_Update$PreviousPreset = {ctor: 'PreviousPreset'};
	var _pablobcb$elm_lead$Main_Update$NextPreset = {ctor: 'NextPreset'};
	var _pablobcb$elm_lead$Main_Update$OnMidiStateChange = function (a) {
		return {ctor: 'OnMidiStateChange', _0: a};
	};
	var _pablobcb$elm_lead$Main_Update$MouseUp = {ctor: 'MouseUp'};
	var _pablobcb$elm_lead$Main_Update$OnScreenKeyboardMsg = function (a) {
		return {ctor: 'OnScreenKeyboardMsg', _0: a};
	};
	var _pablobcb$elm_lead$Main_Update$PanelMsg = function (a) {
		return {ctor: 'PanelMsg', _0: a};
	};
	var _pablobcb$elm_lead$Main_Update$update = F2(
		function (msg, model) {
			var _p0 = msg;
			switch (_p0.ctor) {
				case 'NextPreset':
					return {
						ctor: '_Tuple2',
						_0: model,
						_1: _pablobcb$elm_lead$Port$nextPreset(
							{})
					};
				case 'PreviousPreset':
					return {
						ctor: '_Tuple2',
						_0: model,
						_1: _pablobcb$elm_lead$Port$previousPreset(
							{})
					};
				case 'PresetChange':
					var initModel = A2(_pablobcb$elm_lead$Main_Model$init, _p0._0, model.midiSupport);
					var model$ = _elm_lang$core$Native_Utils.update(
						initModel,
						{midiConnected: model.midiConnected, onScreenKeyboard: model.onScreenKeyboard});
					return {ctor: '_Tuple2', _0: model$, _1: _elm_lang$core$Platform_Cmd$none};
				case 'MouseUp':
					var _p1 = A2(
						_pablobcb$elm_lead$Container_Panel_Update$update,
						_pablobcb$elm_lead$Container_Panel_Update$KnobMsg(_pablobcb$elm_lead$Component_Knob$MouseUp),
						model.panel);
					var updatedPanel = _p1._0;
					var panelCmd = _p1._1;
					var model$ = A2(_pablobcb$elm_lead$Main_Model$updatePanel, updatedPanel, model);
					var _p2 = A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$update, _pablobcb$elm_lead$Container_OnScreenKeyboard_Update$MouseUp, model$.onScreenKeyboard);
					var updatedKbd = _p2._0;
					var kbdCmd = _p2._1;
					var model$$ = A2(_pablobcb$elm_lead$Main_Model$updateOnScreenKeyboard, updatedKbd, model$);
					return {
						ctor: '_Tuple2',
						_0: model$$,
						_1: A2(
							_elm_lang$core$Platform_Cmd$map,
							_elm_lang$core$Basics$always(_pablobcb$elm_lead$Main_Update$MouseUp),
							_elm_lang$core$Platform_Cmd$batch(
								_elm_lang$core$Native_List.fromArray(
									[panelCmd, kbdCmd])))
					};
				case 'PanelMsg':
					var _p3 = A2(_pablobcb$elm_lead$Container_Panel_Update$update, _p0._0, model.panel);
					var updatedPanel = _p3._0;
					var panelCmd = _p3._1;
					return {
						ctor: '_Tuple2',
						_0: A2(_pablobcb$elm_lead$Main_Model$updatePanel, updatedPanel, model),
						_1: A2(_elm_lang$core$Platform_Cmd$map, _pablobcb$elm_lead$Main_Update$PanelMsg, panelCmd)
					};
				case 'OnScreenKeyboardMsg':
					var _p7 = _p0._0;
					var _p4 = function () {
						var _p5 = _p7;
						if (_p5.ctor === 'MidiMessageIn') {
							return {
								ctor: '_Tuple2',
								_0: true,
								_1: A3(
									_elm_lang$core$Task$perform,
									_elm_lang$core$Basics$always(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$NoOp),
									_elm_lang$core$Basics$always(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$NoOp),
									_elm_lang$core$Process$sleep(50))
							};
						} else {
							return {ctor: '_Tuple2', _0: false, _1: _elm_lang$core$Platform_Cmd$none};
						}
					}();
					var midiMsgInLedOn = _p4._0;
					var blinkOffCmd = _p4._1;
					var _p6 = A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$update, _p7, model.onScreenKeyboard);
					var updatedKbd = _p6._0;
					var kbdCmd = _p6._1;
					return {
						ctor: '_Tuple2',
						_0: A2(
							_pablobcb$elm_lead$Main_Model$updateOnScreenKeyboard,
							updatedKbd,
							_elm_lang$core$Native_Utils.update(
								model,
								{midiMsgInLedOn: midiMsgInLedOn})),
						_1: A2(
							_elm_lang$core$Platform_Cmd$map,
							_pablobcb$elm_lead$Main_Update$OnScreenKeyboardMsg,
							_elm_lang$core$Platform_Cmd$batch(
								_elm_lang$core$Native_List.fromArray(
									[blinkOffCmd, kbdCmd])))
					};
				default:
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{midiConnected: _p0._0}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
			}
		});
	
	var _pablobcb$elm_lead$Component_InformationBar$informationBar = function (model) {
		var blinkerClass = A2(
			_elm_lang$core$Basics_ops['++'],
			'midi-indicator__blinker midi-indicator__blinker--',
			model.midiMsgInLedOn ? 'active' : 'inactive');
		var midiIndicatorClass = A2(
			_elm_lang$core$Basics_ops['++'],
			'midi-indicator__status midi-indicator__status--',
			_elm_lang$core$Basics$not(model.midiSupport) ? 'not-supported' : (model.midiConnected ? 'active' : 'inactive'));
		return A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$class('information-bar')
				]),
			_elm_lang$core$Native_List.fromArray(
				[
					A4(
					_pablobcb$elm_lead$Component_Incrementer$incrementer,
					_pablobcb$elm_lead$Component_Incrementer$Octave,
					A2(
						_elm_lang$core$Basics_ops['++'],
						' C',
						_elm_lang$core$Basics$toString(model.onScreenKeyboard.octave)),
					_pablobcb$elm_lead$Main_Update$OnScreenKeyboardMsg(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$OctaveDown),
					_pablobcb$elm_lead$Main_Update$OnScreenKeyboardMsg(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$OctaveUp)),
					A4(
					_pablobcb$elm_lead$Component_Incrementer$incrementer,
					_pablobcb$elm_lead$Component_Incrementer$Velocity,
					_elm_lang$core$Basics$toString(model.onScreenKeyboard.velocity),
					_pablobcb$elm_lead$Main_Update$OnScreenKeyboardMsg(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$VelocityDown),
					_pablobcb$elm_lead$Main_Update$OnScreenKeyboardMsg(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$VelocityUp)),
					A4(
					_pablobcb$elm_lead$Component_Incrementer$incrementer,
					_pablobcb$elm_lead$Component_Incrementer$Patch,
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(model.presetId),
						A2(_elm_lang$core$Basics_ops['++'], '  ', model.presetName)),
					_pablobcb$elm_lead$Main_Update$PreviousPreset,
					_pablobcb$elm_lead$Main_Update$NextPreset),
					A2(
					_elm_lang$html$Html$div,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('midi-indicator')
						]),
					_elm_lang$core$Native_List.fromArray(
						[
							A2(
							_elm_lang$html$Html$div,
							_elm_lang$core$Native_List.fromArray(
								[
									_elm_lang$html$Html_Attributes$class(blinkerClass)
								]),
							_elm_lang$core$Native_List.fromArray(
								[])),
							A2(
							_elm_lang$html$Html$div,
							_elm_lang$core$Native_List.fromArray(
								[
									_elm_lang$html$Html_Attributes$class(midiIndicatorClass)
								]),
							_elm_lang$core$Native_List.fromArray(
								[]))
						]))
				]));
	};
	
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_View$getKeyClass = F4(
		function (model, noteName, midiNote, highlight) {
			var visibily = ((_elm_lang$core$Native_Utils.cmp(midiNote, 12) < 0) || (_elm_lang$core$Native_Utils.cmp(midiNote, 120) > 0)) ? 'keyboard__key--full' : (((_elm_lang$core$Native_Utils.cmp(midiNote, 24) < 0) || (_elm_lang$core$Native_Utils.cmp(midiNote, 108) > 0)) ? 'keyboard__key--big' : (((_elm_lang$core$Native_Utils.cmp(midiNote, 48) < 0) || (_elm_lang$core$Native_Utils.cmp(midiNote, 96) > 0)) ? 'keyboard__key--medium' : ''));
			var currentOctave = highlight ? 'keyboard__key--current-octave' : '';
			var note = (_elm_lang$core$Native_Utils.cmp(
				_elm_lang$core$String$length(noteName),
				1) > 0) ? '' : A2(_elm_lang$core$Basics_ops['++'], 'keyboard__key--', noteName);
			var keyPressed = function () {
				var mouseNote = function () {
					var _p0 = model.mousePressedNote;
					if (_p0.ctor === 'Just') {
						return _elm_lang$core$Native_List.fromArray(
							[_p0._0]);
					} else {
						return _elm_lang$core$Native_List.fromArray(
							[]);
					}
				}();
				return A2(
					_elm_lang$core$List$member,
					midiNote,
					A2(
						_elm_lang$core$Basics_ops['++'],
						A2(_elm_lang$core$List$map, _elm_lang$core$Basics$snd, model.pressedNotes),
						A2(_elm_lang$core$Basics_ops['++'], model.midiPressedNotes, mouseNote)));
			}() ? 'keyboard__key--pressed' : '';
			var isSharpKey = A2(_elm_lang$core$String$contains, '#', noteName);
			var position = A2(
				_elm_lang$core$Basics_ops['++'],
				'keyboard__key--',
				isSharpKey ? 'higher' : 'lower');
			return A2(
				_elm_lang$core$String$join,
				' ',
				A2(
					_elm_lang$core$List$filter,
					F2(
						function (x, y) {
							return !_elm_lang$core$Native_Utils.eq(x, y);
						})(''),
					_elm_lang$core$Native_List.fromArray(
						['keyboard__key', position, keyPressed, note, currentOctave, visibily])));
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_View$onMouseLeave = function (midiNote) {
		return _elm_lang$html$Html_Events$onMouseLeave(
			_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$MouseLeave(midiNote));
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_View$onMouseEnter = function (midiNote) {
		return _elm_lang$html$Html_Events$onMouseEnter(
			_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$MouseEnter(midiNote));
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_View$key = F4(
		function (model, noteName, midiNote, octave) {
			var isCurrentOctave = _elm_lang$core$Native_Utils.eq(model.octave, octave) || (_elm_lang$core$Native_Utils.eq(model.octave, octave - 1) && A2(
				_elm_lang$core$List$member,
				noteName,
				_elm_lang$core$Native_List.fromArray(
					['c', 'c#', 'd', 'd#'])));
			var classes = A4(_pablobcb$elm_lead$Container_OnScreenKeyboard_View$getKeyClass, model, noteName, midiNote, isCurrentOctave);
			return A2(
				_elm_lang$html$Html$li,
				_elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$html$Html_Attributes$class(classes),
						_pablobcb$elm_lead$Container_OnScreenKeyboard_View$onMouseEnter(midiNote),
						_pablobcb$elm_lead$Container_OnScreenKeyboard_View$onMouseLeave(midiNote)
					]),
				_elm_lang$core$Native_List.fromArray(
					[]));
		});
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_View$octaveKeys = _elm_lang$core$Native_List.fromArray(
		['c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#', 'a', 'a#', 'b']);
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_View$onScreenKeyboardKeys = A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$List$concat(
			A2(_elm_lang$core$List$repeat, 10, _pablobcb$elm_lead$Container_OnScreenKeyboard_View$octaveKeys)),
		A2(_elm_lang$core$List$take, 8, _pablobcb$elm_lead$Container_OnScreenKeyboard_View$octaveKeys));
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_View$view = function (model) {
		var keys = A4(
			_elm_lang$core$List$map3,
			_pablobcb$elm_lead$Container_OnScreenKeyboard_View$key(model),
			_pablobcb$elm_lead$Container_OnScreenKeyboard_View$onScreenKeyboardKeys,
			_elm_lang$core$Native_List.range(0, 127),
			_pablobcb$elm_lead$Midi$midiNoteOctaves);
		return A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$class('virtual-keyboard')
				]),
			_elm_lang$core$Native_List.fromArray(
				[
					A2(
					_elm_lang$html$Html$ul,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('keyboard'),
							_elm_lang$html$Html_Events$onMouseDown(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$MouseDown)
						]),
					keys)
				]));
	};
	var _pablobcb$elm_lead$Container_OnScreenKeyboard_View$keyboard = F2(
		function (keyboardMsg, model) {
			return A2(
				_elm_lang$html$Html_App$map,
				keyboardMsg,
				_pablobcb$elm_lead$Container_OnScreenKeyboard_View$view(model));
		});
	
	var _pablobcb$elm_lead$Container_Panel_View$instructions = function () {
		var instructions = A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Native_List.fromArray(
				['octave down', 'octave up', 'velocity down', 'velocity up']),
			A2(
				_elm_lang$core$List$map,
				F2(
					function (x, y) {
						return A2(_elm_lang$core$Basics_ops['++'], x, y);
					})('play '),
				_elm_lang$core$Native_List.fromArray(
					['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B', 'C 8va', 'C# 8va', 'D 8va', 'D# 8va'])));
		var hotKeys = _elm_lang$core$Native_List.fromArray(
			['Z', 'X', 'C', 'V', 'A', 'W', 'S', 'E', 'D', 'F', 'T', 'G', 'Y', 'H', 'U', 'J', 'K', 'O', 'L', 'P']);
		return A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$class('panel-controls__instructions')
				]),
			_elm_lang$core$Native_List.fromArray(
				[
					A2(
					_elm_lang$html$Html$span,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('instructions__title')
						]),
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html$text('INSTRUCTIONS')
						])),
					A2(
					_elm_lang$html$Html$table,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('instructions')
						]),
					A3(
						_elm_lang$core$List$map2,
						F2(
							function (hotkey, instruction) {
								return A2(
									_elm_lang$html$Html$tr,
									_elm_lang$core$Native_List.fromArray(
										[
											_elm_lang$html$Html_Attributes$class('instructions__entry')
										]),
									_elm_lang$core$Native_List.fromArray(
										[
											A2(
											_elm_lang$html$Html$td,
											_elm_lang$core$Native_List.fromArray(
												[]),
											_elm_lang$core$Native_List.fromArray(
												[
													_elm_lang$html$Html$text(hotkey)
												])),
											A2(
											_elm_lang$html$Html$td,
											_elm_lang$core$Native_List.fromArray(
												[
													_elm_lang$html$Html_Attributes$class('instructions__label')
												]),
											_elm_lang$core$Native_List.fromArray(
												[
													_elm_lang$html$Html$text(instruction)
												]))
										]));
							}),
						hotKeys,
						instructions))
				]));
	}();
	var _pablobcb$elm_lead$Container_Panel_View$column = function (content) {
		return A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$class('panel-controls__column')
				]),
			content);
	};
	var _pablobcb$elm_lead$Container_Panel_View$section = F3(
		function (bevelType, title, content) {
			var sectionClass = function () {
				var _p0 = bevelType;
				if (_p0.ctor === 'WithBevel') {
					return 'section section--with-bevel';
				} else {
					return 'section';
				}
			}();
			return A2(
				_elm_lang$html$Html$div,
				_elm_lang$core$Native_List.fromArray(
					[
						_elm_lang$html$Html_Attributes$class(sectionClass)
					]),
				_elm_lang$core$Native_List.fromArray(
					[
						A2(
						_elm_lang$html$Html$div,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html_Attributes$class('section__title')
							]),
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html$text(title)
							])),
						A2(
						_elm_lang$html$Html$div,
						_elm_lang$core$Native_List.fromArray(
							[
								_elm_lang$html$Html_Attributes$class('section__content')
							]),
						content)
					]));
		});
	var _pablobcb$elm_lead$Container_Panel_View$nordKnob = F2(
		function (model, knobInstance) {
			var knob = _elm_lang$core$List$head(
				A2(
					_elm_lang$core$List$filter,
					function (knob$) {
						return _elm_lang$core$Native_Utils.eq(knob$.idKey, knobInstance);
					},
					model.knobs));
			var _p1 = knob;
			if (_p1.ctor === 'Nothing') {
				return _elm_lang$core$Native_Utils.crashCase(
					'Container.Panel.View',
					{
						start: {line: 28, column: 9},
						end: {line: 33, column: 51}
					},
					_p1)('inexistent knob identifier');
			} else {
				return A2(_pablobcb$elm_lead$Component_Knob$knob, _pablobcb$elm_lead$Container_Panel_Update$KnobMsg, _p1._0);
			}
		});
	var _pablobcb$elm_lead$Container_Panel_View$osc1 = function (model) {
		return A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$class('oscillators__osc1')
				]),
			_elm_lang$core$Native_List.fromArray(
				[
					A3(_pablobcb$elm_lead$Component_OptionPicker$optionPicker, 'Waveform', _pablobcb$elm_lead$Container_Panel_Update$Osc1WaveformChange, model.osc1WaveformBtn),
					A2(
					_elm_lang$html$Html$span,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('oscillators__label')
						]),
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html$text('OSC 1')
						])),
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$FM)
				]));
	};
	var _pablobcb$elm_lead$Container_Panel_View$osc2 = function (model) {
		return A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$class('oscillators__osc2')
				]),
			_elm_lang$core$Native_List.fromArray(
				[
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$Osc2Semitone),
					A3(_pablobcb$elm_lead$Component_OptionPicker$optionPicker, 'Waveform', _pablobcb$elm_lead$Container_Panel_Update$Osc2WaveformChange, model.osc2WaveformBtn),
					A2(
					_elm_lang$html$Html$span,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('oscillators__label')
						]),
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html$text('OSC 2')
						])),
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$Osc2Detune),
					A3(_pablobcb$elm_lead$Component_Switch$switch, 'kbd track', _pablobcb$elm_lead$Container_Panel_Update$Osc2KbdTrackToggle, model.osc2KbdTrackSwitch)
				]));
	};
	var _pablobcb$elm_lead$Container_Panel_View$WithoutBevel = {ctor: 'WithoutBevel'};
	var _pablobcb$elm_lead$Container_Panel_View$amplifier = function (model) {
		return A3(
			_pablobcb$elm_lead$Container_Panel_View$section,
			_pablobcb$elm_lead$Container_Panel_View$WithoutBevel,
			'amplifier',
			_elm_lang$core$Native_List.fromArray(
				[
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$AmpAttack),
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$AmpDecay),
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$AmpSustain),
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$AmpRelease),
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$AmpGain)
				]));
	};
	var _pablobcb$elm_lead$Container_Panel_View$filter = function (model) {
		return A3(
			_pablobcb$elm_lead$Container_Panel_View$section,
			_pablobcb$elm_lead$Container_Panel_View$WithoutBevel,
			'filter',
			_elm_lang$core$Native_List.fromArray(
				[
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$FilterAttack),
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$FilterDecay),
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$FilterSustain),
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$FilterRelease),
					A3(_pablobcb$elm_lead$Component_OptionPicker$optionPicker, 'Filter Type', _pablobcb$elm_lead$Container_Panel_Update$FilterTypeChange, model.filterTypeBtn),
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$FilterCutoff),
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$FilterQ),
					A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$FilterEnvelopeAmount),
					A3(_pablobcb$elm_lead$Component_Switch$switch, 'distortion', _pablobcb$elm_lead$Container_Panel_Update$OverdriveToggle, model.overdriveSwitch)
				]));
	};
	var _pablobcb$elm_lead$Container_Panel_View$WithBevel = {ctor: 'WithBevel'};
	var _pablobcb$elm_lead$Container_Panel_View$oscillatorSection = function (model) {
		return A3(
			_pablobcb$elm_lead$Container_Panel_View$section,
			_pablobcb$elm_lead$Container_Panel_View$WithBevel,
			'oscillators',
			_elm_lang$core$Native_List.fromArray(
				[
					_pablobcb$elm_lead$Container_Panel_View$osc1(model),
					_pablobcb$elm_lead$Container_Panel_View$osc2(model),
					A2(
					_elm_lang$html$Html$div,
					_elm_lang$core$Native_List.fromArray(
						[
							_elm_lang$html$Html_Attributes$class('oscillators__extra')
						]),
					_elm_lang$core$Native_List.fromArray(
						[
							A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$PW),
							A2(_pablobcb$elm_lead$Container_Panel_View$nordKnob, model, _pablobcb$elm_lead$Component_Knob$OscMix)
						]))
				]));
	};
	var _pablobcb$elm_lead$Container_Panel_View$view = function (model) {
		return A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$class('panel-controls')
				]),
			_elm_lang$core$Native_List.fromArray(
				[
					_pablobcb$elm_lead$Container_Panel_View$column(
					_elm_lang$core$Native_List.fromArray(
						[
							_pablobcb$elm_lead$Container_Panel_View$oscillatorSection(model)
						])),
					_pablobcb$elm_lead$Container_Panel_View$column(
					_elm_lang$core$Native_List.fromArray(
						[
							_pablobcb$elm_lead$Container_Panel_View$amplifier(model),
							_pablobcb$elm_lead$Container_Panel_View$filter(model)
						])),
					_pablobcb$elm_lead$Container_Panel_View$instructions
				]));
	};
	var _pablobcb$elm_lead$Container_Panel_View$panel = F2(
		function (panelMsg, model) {
			return A2(
				_elm_lang$html$Html_App$map,
				panelMsg,
				_pablobcb$elm_lead$Container_Panel_View$view(model));
		});
	
	var _pablobcb$elm_lead$Main_View$view = function (model) {
		return A2(
			_elm_lang$html$Html$div,
			_elm_lang$core$Native_List.fromArray(
				[
					_elm_lang$html$Html_Attributes$class('dashboard')
				]),
			_elm_lang$core$Native_List.fromArray(
				[
					A2(_pablobcb$elm_lead$Container_Panel_View$panel, _pablobcb$elm_lead$Main_Update$PanelMsg, model.panel),
					A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_View$keyboard, _pablobcb$elm_lead$Main_Update$OnScreenKeyboardMsg, model.onScreenKeyboard),
					_pablobcb$elm_lead$Component_InformationBar$informationBar(model)
				]));
	};
	
	var _pablobcb$elm_lead$Main$subscriptions = function (model) {
		return _elm_lang$core$Platform_Sub$batch(
			_elm_lang$core$Native_List.fromArray(
				[
					_pablobcb$elm_lead$Port$midiIn(
					function (_p0) {
						return _pablobcb$elm_lead$Main_Update$OnScreenKeyboardMsg(
							_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$MidiMessageIn(_p0));
					}),
					_pablobcb$elm_lead$Port$midiStateChange(_pablobcb$elm_lead$Main_Update$OnMidiStateChange),
					_pablobcb$elm_lead$Port$presetChange(_pablobcb$elm_lead$Main_Update$PresetChange),
					_pablobcb$elm_lead$Port$panic(
					_elm_lang$core$Basics$always(
						_pablobcb$elm_lead$Main_Update$OnScreenKeyboardMsg(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$Panic))),
					_elm_lang$keyboard$Keyboard$ups(
					_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$handleKeyUp(_pablobcb$elm_lead$Main_Update$OnScreenKeyboardMsg)),
					_elm_lang$keyboard$Keyboard$downs(
					A2(_pablobcb$elm_lead$Container_OnScreenKeyboard_Update$handleKeyDown, _pablobcb$elm_lead$Main_Update$OnScreenKeyboardMsg, model.onScreenKeyboard)),
					_elm_lang$mouse$Mouse$ups(
					_elm_lang$core$Basics$always(_pablobcb$elm_lead$Main_Update$MouseUp)),
					_elm_lang$mouse$Mouse$moves(
					function (_p1) {
						var _p2 = _p1;
						return _pablobcb$elm_lead$Main_Update$PanelMsg(
							_pablobcb$elm_lead$Container_Panel_Update$KnobMsg(
								_pablobcb$elm_lead$Component_Knob$MouseMove(_p2.y)));
					})
				]));
	};
	var _pablobcb$elm_lead$Main$main = {
		main: _elm_lang$html$Html_App$programWithFlags(
			{
				init: function (flags) {
					return {
						ctor: '_Tuple2',
						_0: A2(_pablobcb$elm_lead$Main_Model$init, flags.preset, flags.midiSupport),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				},
				view: _pablobcb$elm_lead$Main_View$view,
				update: _pablobcb$elm_lead$Main_Update$update,
				subscriptions: _pablobcb$elm_lead$Main$subscriptions
			}),
		flags: A2(
			_elm_lang$core$Json_Decode$andThen,
			A2(_elm_lang$core$Json_Decode_ops[':='], 'midiSupport', _elm_lang$core$Json_Decode$bool),
			function (midiSupport) {
				return A2(
					_elm_lang$core$Json_Decode$andThen,
					A2(
						_elm_lang$core$Json_Decode_ops[':='],
						'preset',
						A2(
							_elm_lang$core$Json_Decode$andThen,
							A2(
								_elm_lang$core$Json_Decode_ops[':='],
								'amp',
								A2(
									_elm_lang$core$Json_Decode$andThen,
									A2(
										_elm_lang$core$Json_Decode_ops[':='],
										'adsr',
										A2(
											_elm_lang$core$Json_Decode$andThen,
											A2(_elm_lang$core$Json_Decode_ops[':='], 'attack', _elm_lang$core$Json_Decode$int),
											function (attack) {
												return A2(
													_elm_lang$core$Json_Decode$andThen,
													A2(_elm_lang$core$Json_Decode_ops[':='], 'decay', _elm_lang$core$Json_Decode$int),
													function (decay) {
														return A2(
															_elm_lang$core$Json_Decode$andThen,
															A2(_elm_lang$core$Json_Decode_ops[':='], 'release', _elm_lang$core$Json_Decode$int),
															function (release) {
																return A2(
																	_elm_lang$core$Json_Decode$andThen,
																	A2(_elm_lang$core$Json_Decode_ops[':='], 'sustain', _elm_lang$core$Json_Decode$int),
																	function (sustain) {
																		return _elm_lang$core$Json_Decode$succeed(
																			{attack: attack, decay: decay, release: release, sustain: sustain});
																	});
															});
													});
											})),
									function (adsr) {
										return A2(
											_elm_lang$core$Json_Decode$andThen,
											A2(_elm_lang$core$Json_Decode_ops[':='], 'masterVolume', _elm_lang$core$Json_Decode$int),
											function (masterVolume) {
												return _elm_lang$core$Json_Decode$succeed(
													{adsr: adsr, masterVolume: masterVolume});
											});
									})),
							function (amp) {
								return A2(
									_elm_lang$core$Json_Decode$andThen,
									A2(
										_elm_lang$core$Json_Decode_ops[':='],
										'filter',
										A2(
											_elm_lang$core$Json_Decode$andThen,
											A2(
												_elm_lang$core$Json_Decode_ops[':='],
												'adsr',
												A2(
													_elm_lang$core$Json_Decode$andThen,
													A2(_elm_lang$core$Json_Decode_ops[':='], 'attack', _elm_lang$core$Json_Decode$int),
													function (attack) {
														return A2(
															_elm_lang$core$Json_Decode$andThen,
															A2(_elm_lang$core$Json_Decode_ops[':='], 'decay', _elm_lang$core$Json_Decode$int),
															function (decay) {
																return A2(
																	_elm_lang$core$Json_Decode$andThen,
																	A2(_elm_lang$core$Json_Decode_ops[':='], 'release', _elm_lang$core$Json_Decode$int),
																	function (release) {
																		return A2(
																			_elm_lang$core$Json_Decode$andThen,
																			A2(_elm_lang$core$Json_Decode_ops[':='], 'sustain', _elm_lang$core$Json_Decode$int),
																			function (sustain) {
																				return _elm_lang$core$Json_Decode$succeed(
																					{attack: attack, decay: decay, release: release, sustain: sustain});
																			});
																	});
															});
													})),
											function (adsr) {
												return A2(
													_elm_lang$core$Json_Decode$andThen,
													A2(_elm_lang$core$Json_Decode_ops[':='], 'envelopeAmount', _elm_lang$core$Json_Decode$int),
													function (envelopeAmount) {
														return A2(
															_elm_lang$core$Json_Decode$andThen,
															A2(_elm_lang$core$Json_Decode_ops[':='], 'frequency', _elm_lang$core$Json_Decode$int),
															function (frequency) {
																return A2(
																	_elm_lang$core$Json_Decode$andThen,
																	A2(_elm_lang$core$Json_Decode_ops[':='], 'q', _elm_lang$core$Json_Decode$int),
																	function (q) {
																		return A2(
																			_elm_lang$core$Json_Decode$andThen,
																			A2(_elm_lang$core$Json_Decode_ops[':='], 'type_', _elm_lang$core$Json_Decode$string),
																			function (type_) {
																				return _elm_lang$core$Json_Decode$succeed(
																					{adsr: adsr, envelopeAmount: envelopeAmount, frequency: frequency, q: q, type_: type_});
																			});
																	});
															});
													});
											})),
									function (filter) {
										return A2(
											_elm_lang$core$Json_Decode$andThen,
											A2(_elm_lang$core$Json_Decode_ops[':='], 'name', _elm_lang$core$Json_Decode$string),
											function (name) {
												return A2(
													_elm_lang$core$Json_Decode$andThen,
													A2(
														_elm_lang$core$Json_Decode_ops[':='],
														'oscs',
														A2(
															_elm_lang$core$Json_Decode$andThen,
															A2(_elm_lang$core$Json_Decode_ops[':='], 'mix', _elm_lang$core$Json_Decode$int),
															function (mix) {
																return A2(
																	_elm_lang$core$Json_Decode$andThen,
																	A2(
																		_elm_lang$core$Json_Decode_ops[':='],
																		'osc1',
																		A2(
																			_elm_lang$core$Json_Decode$andThen,
																			A2(_elm_lang$core$Json_Decode_ops[':='], 'fmGain', _elm_lang$core$Json_Decode$int),
																			function (fmGain) {
																				return A2(
																					_elm_lang$core$Json_Decode$andThen,
																					A2(_elm_lang$core$Json_Decode_ops[':='], 'waveformType', _elm_lang$core$Json_Decode$string),
																					function (waveformType) {
																						return _elm_lang$core$Json_Decode$succeed(
																							{fmGain: fmGain, waveformType: waveformType});
																					});
																			})),
																	function (osc1) {
																		return A2(
																			_elm_lang$core$Json_Decode$andThen,
																			A2(
																				_elm_lang$core$Json_Decode_ops[':='],
																				'osc2',
																				A2(
																					_elm_lang$core$Json_Decode$andThen,
																					A2(_elm_lang$core$Json_Decode_ops[':='], 'detune', _elm_lang$core$Json_Decode$int),
																					function (detune) {
																						return A2(
																							_elm_lang$core$Json_Decode$andThen,
																							A2(_elm_lang$core$Json_Decode_ops[':='], 'kbdTrack', _elm_lang$core$Json_Decode$bool),
																							function (kbdTrack) {
																								return A2(
																									_elm_lang$core$Json_Decode$andThen,
																									A2(_elm_lang$core$Json_Decode_ops[':='], 'semitone', _elm_lang$core$Json_Decode$int),
																									function (semitone) {
																										return A2(
																											_elm_lang$core$Json_Decode$andThen,
																											A2(_elm_lang$core$Json_Decode_ops[':='], 'waveformType', _elm_lang$core$Json_Decode$string),
																											function (waveformType) {
																												return _elm_lang$core$Json_Decode$succeed(
																													{detune: detune, kbdTrack: kbdTrack, semitone: semitone, waveformType: waveformType});
																											});
																									});
																							});
																					})),
																			function (osc2) {
																				return A2(
																					_elm_lang$core$Json_Decode$andThen,
																					A2(_elm_lang$core$Json_Decode_ops[':='], 'pw', _elm_lang$core$Json_Decode$int),
																					function (pw) {
																						return _elm_lang$core$Json_Decode$succeed(
																							{mix: mix, osc1: osc1, osc2: osc2, pw: pw});
																					});
																			});
																	});
															})),
													function (oscs) {
														return A2(
															_elm_lang$core$Json_Decode$andThen,
															A2(_elm_lang$core$Json_Decode_ops[':='], 'overdrive', _elm_lang$core$Json_Decode$bool),
															function (overdrive) {
																return A2(
																	_elm_lang$core$Json_Decode$andThen,
																	A2(_elm_lang$core$Json_Decode_ops[':='], 'presetId', _elm_lang$core$Json_Decode$int),
																	function (presetId) {
																		return _elm_lang$core$Json_Decode$succeed(
																			{amp: amp, filter: filter, name: name, oscs: oscs, overdrive: overdrive, presetId: presetId});
																	});
															});
													});
											});
									});
							})),
					function (preset) {
						return _elm_lang$core$Json_Decode$succeed(
							{midiSupport: midiSupport, preset: preset});
					});
			})
	};
	
	var Elm = {};
	Elm['Main'] = Elm['Main'] || {};
	_elm_lang$core$Native_Platform.addPublicModule(Elm['Main'], 'Main', typeof _pablobcb$elm_lead$Main$main === 'undefined' ? null : _pablobcb$elm_lead$Main$main);
	
	if ("function" === "function" && __webpack_require__(38)['amd'])
	{
	  !(__WEBPACK_AMD_DEFINE_ARRAY__ = [], __WEBPACK_AMD_DEFINE_RESULT__ = function() { return Elm; }.apply(exports, __WEBPACK_AMD_DEFINE_ARRAY__), __WEBPACK_AMD_DEFINE_RESULT__ !== undefined && (module.exports = __WEBPACK_AMD_DEFINE_RESULT__));
	  return;
	}
	
	if (true)
	{
	  module['exports'] = Elm;
	  return;
	}
	
	var globalElm = this['Elm'];
	if (typeof globalElm === "undefined")
	{
	  this['Elm'] = Elm;
	  return;
	}
	
	for (var publicModule in Elm)
	{
	  if (publicModule in globalElm)
	  {
	    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
	  }
	  globalElm[publicModule] = Elm[publicModule];
	}
	
	}).call(this);
	
	
	/* WEBPACK VAR INJECTION */}.call(exports, __webpack_require__(39)(module)))

/***/ },
/* 12 */
/***/ function(module, exports, __webpack_require__) {

	module.exports = __webpack_require__.p + "58045dabdc3a361cb9bb9faf2f1dd1f3.ttf";

/***/ },
/* 13 */
/***/ function(module, exports, __webpack_require__) {

	module.exports = __webpack_require__.p + "b8aa4dce3ad085de94f02cae44777c6e.woff";

/***/ },
/* 14 */
/***/ function(module, exports) {

	module.exports = [
		{
			"name": "fst",
			"presetId": 1,
			"filter": {
				"type_": "lowpass",
				"frequency": 127,
				"q": 10,
				"envelopeAmount": 0,
				"adsr": {
					"attack": 0,
					"decay": 0,
					"sustain": 0,
					"release": 0
				}
			},
			"amp": {
				"adsr": {
					"attack": 0,
					"decay": 0,
					"sustain": 127,
					"release": 0
				},
				"masterVolume": 20
			},
			"oscs": {
				"osc1": {
					"waveformType": "sawtooth",
					"fmGain": 0
				},
				"osc2": {
					"waveformType": "sawtooth",
					"semitone": 0,
					"detune": 0,
					"kbdTrack": true
				},
				"pw": 0,
				"mix": 0
			},
			"overdrive": false
		},
		{
			"name": "snd",
			"filter": {
				"type_": "bandpass",
				"frequency": 127,
				"q": 10,
				"envelopeAmount": 0,
				"adsr": {
					"attack": 0,
					"decay": 0,
					"sustain": 0,
					"release": 0
				}
			},
			"amp": {
				"adsr": {
					"attack": 0,
					"decay": 0,
					"sustain": 127,
					"release": 0
				},
				"masterVolume": 20
			},
			"oscs": {
				"osc1": {
					"waveformType": "sine",
					"fmGain": 60
				},
				"osc2": {
					"waveformType": "sawtooth",
					"semitone": 0,
					"detune": 0,
					"kbdTrack": true
				},
				"pw": 0,
				"mix": 0
			},
			"overdrive": false
		},
		{
			"name": "snd",
			"filter": {
				"type_": "bandpass",
				"frequency": 127,
				"q": 10,
				"envelopeAmount": 0,
				"adsr": {
					"attack": 0,
					"decay": 0,
					"sustain": 0,
					"release": 0
				}
			},
			"amp": {
				"adsr": {
					"attack": 0,
					"decay": 0,
					"sustain": 127,
					"release": 0
				},
				"masterVolume": 20
			},
			"oscs": {
				"osc1": {
					"waveformType": "sine",
					"fmGain": 0
				},
				"osc2": {
					"waveformType": "whitenoise",
					"semitone": 0,
					"detune": 0,
					"kbdTrack": true
				},
				"pw": 0,
				"mix": 1
			},
			"overdrive": false
		}
	];

/***/ },
/* 15 */
/***/ function(module, exports, __webpack_require__) {

	// style-loader: Adds some css to the DOM by adding a <style> tag
	
	// load the styles
	var content = __webpack_require__(9);
	if(typeof content === 'string') content = [[module.id, content, '']];
	// add the styles to the DOM
	var update = __webpack_require__(5)(content, {});
	if(content.locals) module.exports = content.locals;
	// Hot Module Replacement
	if(false) {
		// When the styles change, update the <style> tags
		if(!content.locals) {
			module.hot.accept("!!./../css-loader/index.js!./../postcss-loader/index.js!./normalize.css", function() {
				var newContent = require("!!./../css-loader/index.js!./../postcss-loader/index.js!./normalize.css");
				if(typeof newContent === 'string') newContent = [[module.id, newContent, '']];
				update(newContent);
			});
		}
		// When the module is disposed, remove the <style> tags
		module.hot.dispose(function() { update(); });
	}

/***/ },
/* 16 */
/***/ function(module, exports, __webpack_require__) {

	// style-loader: Adds some css to the DOM by adding a <style> tag
	
	// load the styles
	var content = __webpack_require__(10);
	if(typeof content === 'string') content = [[module.id, content, '']];
	// add the styles to the DOM
	var update = __webpack_require__(5)(content, {});
	if(content.locals) module.exports = content.locals;
	// Hot Module Replacement
	if(false) {
		// When the styles change, update the <style> tags
		if(!content.locals) {
			module.hot.accept("!!./../../node_modules/css-loader/index.js!./../../node_modules/postcss-loader/index.js!./../../node_modules/stylus-loader/index.js!./../../node_modules/stylint-loader/index.js!./main.styl", function() {
				var newContent = require("!!./../../node_modules/css-loader/index.js!./../../node_modules/postcss-loader/index.js!./../../node_modules/stylus-loader/index.js!./../../node_modules/stylint-loader/index.js!./main.styl");
				if(typeof newContent === 'string') newContent = [[module.id, newContent, '']];
				update(newContent);
			});
		}
		// When the module is disposed, remove the <style> tags
		module.hot.dispose(function() { update(); });
	}

/***/ },
/* 17 */
/***/ function(module, exports) {

	module.exports = "\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8' standalone='no'?%3E %3C!-- Created with Inkscape (http://www.inkscape.org/) --%3E %3Csvg xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:cc='http://creativecommons.org/ns%23' xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns%23' xmlns:svg='http://www.w3.org/2000/svg' xmlns='http://www.w3.org/2000/svg' xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd' xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape' width='25.578321' height='12.02346' viewBox='0 0 25.578321 12.02346' id='svg2' version='1.1' inkscape:version='0.91 r13725' sodipodi:docname='attack.svg'%3E %3Cdefs id='defs4' /%3E %3Csodipodi:namedview id='base' pagecolor='%23767676' bordercolor='%23ffffff' borderopacity='1' inkscape:pageopacity='0' inkscape:pageshadow='2' inkscape:zoom='22.627418' inkscape:cx='15.427557' inkscape:cy='10.202857' inkscape:document-units='px' inkscape:current-layer='layer1' showgrid='true' borderlayer='true' inkscape:showpageshadow='true' inkscape:window-width='1920' inkscape:window-height='1001' inkscape:window-x='0' inkscape:window-y='27' inkscape:window-maximized='1' inkscape:snap-grids='true' fit-margin-top='0' fit-margin-left='0' fit-margin-right='0' fit-margin-bottom='0' units='px'%3E %3Cinkscape:grid type='xygrid' id='grid3336' color='%23b4bc45' opacity='0.1254902' originx='-122.9929' originy='-2128.9875' /%3E %3C/sodipodi:namedview%3E %3Cmetadata id='metadata7'%3E %3Crdf:RDF%3E %3Ccc:Work rdf:about=''%3E %3Cdc:format%3Eimage/svg+xml%3C/dc:format%3E %3Cdc:type rdf:resource='http://purl.org/dc/dcmitype/StillImage' /%3E %3Cdc:title%3E%3C/dc:title%3E %3C/cc:Work%3E %3C/rdf:RDF%3E %3C/metadata%3E %3Cg inkscape:label='Layer 1' inkscape:groupmode='layer' id='layer1' transform='translate(-113.49295,89.155926)'%3E %3Cpath style='opacity:1;fill:%23ffffff;fill-opacity:1;fill-rule:nonzero;stroke:%234d4d4d;stroke-width:0.50431865;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0.19444448' d='' id='path3348' inkscape:connector-curvature='0' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23787e9f;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 133.56118,-85.083165 c 0.86637,2.733835 1.81205,5.405611 4.49585,6.717115 l 0,0' id='path4983' inkscape:connector-curvature='0' sodipodi:nodetypes='ccc' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23787e9f;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 126.50005,-85.144855 7,0' id='path4979' inkscape:connector-curvature='0' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23787e9f;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 122.50005,-88.144855 c 1.02705,1.26253 2.15887,2.435254 4,3 l 0,0' id='path4308' inkscape:connector-curvature='0' sodipodi:nodetypes='ccc' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23ffffff;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 114.50005,-78.144855 c 2.03357,-3.570745 4.12089,-7.121335 8,-10 l 0,0' id='path4306' inkscape:connector-curvature='0' sodipodi:nodetypes='ccc' /%3E %3C/g%3E %3C/svg%3E\""

/***/ },
/* 18 */
/***/ function(module, exports) {

	module.exports = "\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8' standalone='no'?%3E %3C!-- Created with Inkscape (http://www.inkscape.org/) --%3E %3Csvg xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:cc='http://creativecommons.org/ns%23' xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns%23' xmlns:svg='http://www.w3.org/2000/svg' xmlns='http://www.w3.org/2000/svg' xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd' xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape' width='25.578321' height='12.02346' viewBox='0 0 25.578321 12.02346' id='svg2' version='1.1' inkscape:version='0.91 r13725' sodipodi:docname='decay.svg'%3E %3Cdefs id='defs4' /%3E %3Csodipodi:namedview id='base' pagecolor='%23767676' bordercolor='%23ffffff' borderopacity='1' inkscape:pageopacity='0' inkscape:pageshadow='2' inkscape:zoom='22.627418' inkscape:cx='15.427557' inkscape:cy='10.202857' inkscape:document-units='px' inkscape:current-layer='layer1' showgrid='true' borderlayer='true' inkscape:showpageshadow='true' inkscape:window-width='1920' inkscape:window-height='1001' inkscape:window-x='0' inkscape:window-y='27' inkscape:window-maximized='1' inkscape:snap-grids='true' fit-margin-top='0' fit-margin-left='0' fit-margin-right='0' fit-margin-bottom='0' units='px'%3E %3Cinkscape:grid type='xygrid' id='grid3336' color='%23b4bc45' opacity='0.1254902' originx='-122.9929' originy='-2128.9875' /%3E %3C/sodipodi:namedview%3E %3Cmetadata id='metadata7'%3E %3Crdf:RDF%3E %3Ccc:Work rdf:about=''%3E %3Cdc:format%3Eimage/svg+xml%3C/dc:format%3E %3Cdc:type rdf:resource='http://purl.org/dc/dcmitype/StillImage' /%3E %3Cdc:title%3E%3C/dc:title%3E %3C/cc:Work%3E %3C/rdf:RDF%3E %3C/metadata%3E %3Cg inkscape:label='Layer 1' inkscape:groupmode='layer' id='layer1' transform='translate(-113.49295,89.155926)'%3E %3Cpath style='opacity:1;fill:%23ffffff;fill-opacity:1;fill-rule:nonzero;stroke:%234d4d4d;stroke-width:0.50431865;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0.19444448' d='' id='path3348' inkscape:connector-curvature='0' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23787e9f;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 114.50005,-78.144855 c 2.03357,-3.570745 4.12089,-7.121335 8,-10 l 0,0' id='path4306' inkscape:connector-curvature='0' sodipodi:nodetypes='ccc' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23787e9f;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 133.56118,-85.083165 c 0.86637,2.733835 1.81205,5.405611 4.49585,6.717115 l 0,0' id='path4983' inkscape:connector-curvature='0' sodipodi:nodetypes='ccc' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23787e9f;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 126.50005,-85.144855 7,0' id='path4979' inkscape:connector-curvature='0' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23ffffff;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 122.50005,-88.144855 c 1.02705,1.26253 2.15887,2.435254 4,3 l 0,0' id='path4308' inkscape:connector-curvature='0' sodipodi:nodetypes='ccc' /%3E %3C/g%3E %3C/svg%3E\""

/***/ },
/* 19 */
/***/ function(module, exports) {

	module.exports = "\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8' standalone='no'?%3E %3Csvg viewBox='0 0 256 250' version='1.1' xmlns='http://www.w3.org/2000/svg'%3E %3Cpath fill='white' d='M128.00106,0 C57.3172926,0 0,57.3066942 0,128.00106 C0,184.555281 36.6761997,232.535542 87.534937,249.460899 C93.9320223,250.645779 96.280588,246.684165 96.280588,243.303333 C96.280588,240.251045 96.1618878,230.167899 96.106777,219.472176 C60.4967585,227.215235 52.9826207,204.369712 52.9826207,204.369712 C47.1599584,189.574598 38.770408,185.640538 38.770408,185.640538 C27.1568785,177.696113 39.6458206,177.859325 39.6458206,177.859325 C52.4993419,178.762293 59.267365,191.04987 59.267365,191.04987 C70.6837675,210.618423 89.2115753,204.961093 96.5158685,201.690482 C97.6647155,193.417512 100.981959,187.77078 104.642583,184.574357 C76.211799,181.33766 46.324819,170.362144 46.324819,121.315702 C46.324819,107.340889 51.3250588,95.9223682 59.5132437,86.9583937 C58.1842268,83.7344152 53.8029229,70.715562 60.7532354,53.0843636 C60.7532354,53.0843636 71.5019501,49.6441813 95.9626412,66.2049595 C106.172967,63.368876 117.123047,61.9465949 128.00106,61.8978432 C138.879073,61.9465949 149.837632,63.368876 160.067033,66.2049595 C184.49805,49.6441813 195.231926,53.0843636 195.231926,53.0843636 C202.199197,70.715562 197.815773,83.7344152 196.486756,86.9583937 C204.694018,95.9223682 209.660343,107.340889 209.660343,121.315702 C209.660343,170.478725 179.716133,181.303747 151.213281,184.472614 C155.80443,188.444828 159.895342,196.234518 159.895342,208.176593 C159.895342,225.303317 159.746968,239.087361 159.746968,243.303333 C159.746968,246.709601 162.05102,250.70089 168.53925,249.443941 C219.370432,232.499507 256,184.536204 256,128.00106 C256,57.3066942 198.691187,0 128.00106,0 Z M47.9405593,182.340212 C47.6586465,182.976105 46.6581745,183.166873 45.7467277,182.730227 C44.8183235,182.312656 44.2968914,181.445722 44.5978808,180.80771 C44.8734344,180.152739 45.876026,179.97045 46.8023103,180.409216 C47.7328342,180.826786 48.2627451,181.702199 47.9405593,182.340212 Z M54.2367892,187.958254 C53.6263318,188.524199 52.4329723,188.261363 51.6232682,187.366874 C50.7860088,186.474504 50.6291553,185.281144 51.2480912,184.70672 C51.8776254,184.140775 53.0349512,184.405731 53.8743302,185.298101 C54.7115892,186.201069 54.8748019,187.38595 54.2367892,187.958254 Z M58.5562413,195.146347 C57.7719732,195.691096 56.4895886,195.180261 55.6968417,194.042013 C54.9125733,192.903764 54.9125733,191.538713 55.713799,190.991845 C56.5086651,190.444977 57.7719732,190.936735 58.5753181,192.066505 C59.3574669,193.22383 59.3574669,194.58888 58.5562413,195.146347 Z M65.8613592,203.471174 C65.1597571,204.244846 63.6654083,204.03712 62.5716717,202.981538 C61.4524999,201.94927 61.1409122,200.484596 61.8446341,199.710926 C62.5547146,198.935137 64.0575422,199.15346 65.1597571,200.200564 C66.2704506,201.230712 66.6095936,202.705984 65.8613592,203.471174 Z M75.3025151,206.281542 C74.9930474,207.284134 73.553809,207.739857 72.1039724,207.313809 C70.6562556,206.875043 69.7087748,205.700761 70.0012857,204.687571 C70.302275,203.678621 71.7478721,203.20382 73.2083069,203.659543 C74.6539041,204.09619 75.6035048,205.261994 75.3025151,206.281542 Z M86.046947,207.473627 C86.0829806,208.529209 84.8535871,209.404622 83.3316829,209.4237 C81.8013,209.457614 80.563428,208.603398 80.5464708,207.564772 C80.5464708,206.498591 81.7483088,205.631657 83.2786917,205.606221 C84.8005962,205.576546 86.046947,206.424403 86.046947,207.473627 Z M96.6021471,207.069023 C96.7844366,208.099171 95.7267341,209.156872 94.215428,209.438785 C92.7295577,209.710099 91.3539086,209.074206 91.1652603,208.052538 C90.9808515,206.996955 92.0576306,205.939253 93.5413813,205.66582 C95.054807,205.402984 96.4092596,206.021919 96.6021471,207.069023 Z' /%3E %3C/svg%3E\""

/***/ },
/* 20 */
/***/ function(module, exports) {

	module.exports = "\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8' standalone='no'?%3E %3C!-- Created with Inkscape (http://www.inkscape.org/) --%3E %3Csvg xmlns:osb='http://www.openswatchbook.org/uri/2009/osb' xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:cc='http://creativecommons.org/ns%23' xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns%23' xmlns:svg='http://www.w3.org/2000/svg' xmlns='http://www.w3.org/2000/svg' xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd' xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape' width='95' height='95' viewBox='0 0 95.000001 95.000001' id='svg2' version='1.1' sodipodi:docname='knob-bg.svg' inkscape:version='0.91 r13725'%3E %3Cdefs id='defs4'%3E %3ClinearGradient id='linearGradient6764' osb:paint='solid'%3E %3Cstop style='stop-color:%23000000;stop-opacity:1;' offset='0' id='stop6766' /%3E %3C/linearGradient%3E %3ClinearGradient id='linearGradient6758' osb:paint='solid'%3E %3Cstop style='stop-color:%23ffffff;stop-opacity:1;' offset='0' id='stop6760' /%3E %3C/linearGradient%3E %3ClinearGradient id='linearGradient6712' osb:paint='solid'%3E %3Cstop style='stop-color:%234d4d4d;stop-opacity:1;' offset='0' id='stop6714' /%3E %3C/linearGradient%3E %3ClinearGradient id='linearGradient6704' osb:paint='gradient'%3E %3Cstop style='stop-color:%23ffffff;stop-opacity:1;' offset='0' id='stop6706' /%3E %3Cstop style='stop-color:%23ffffff;stop-opacity:0;' offset='1' id='stop6708' /%3E %3C/linearGradient%3E %3ClinearGradient id='linearGradient6694' osb:paint='solid'%3E %3Cstop style='stop-color:%23ffffff;stop-opacity:1;' offset='0' id='stop6696' /%3E %3C/linearGradient%3E %3ClinearGradient id='linearGradient6680' osb:paint='gradient'%3E %3Cstop style='stop-color:%23ffffff;stop-opacity:1;' offset='0' id='stop6682' /%3E %3Cstop style='stop-color:%23ffffff;stop-opacity:0;' offset='1' id='stop6684' /%3E %3C/linearGradient%3E %3ClinearGradient id='linearGradient6568' osb:paint='solid'%3E %3Cstop style='stop-color:%23b5b5b5;stop-opacity:1;' offset='0' id='stop6570' /%3E %3C/linearGradient%3E %3C/defs%3E %3Csodipodi:namedview id='base' pagecolor='%237b7b7b' bordercolor='%23666666' borderopacity='1.0' inkscape:pageopacity='0' inkscape:pageshadow='2' inkscape:zoom='45.254834' inkscape:cx='34.104178' inkscape:cy='9.0115891' inkscape:document-units='px' inkscape:current-layer='g6790' showgrid='false' inkscape:snap-object-midpoints='false' inkscape:snap-others='true' inkscape:snap-center='false' inkscape:snap-nodes='true' inkscape:snap-grids='true' inkscape:snap-global='true' inkscape:window-width='1920' inkscape:window-height='1001' inkscape:window-x='0' inkscape:window-y='27' inkscape:window-maximized='1' showborder='true' borderlayer='false' fit-margin-top='0' fit-margin-left='0' fit-margin-right='0' fit-margin-bottom='0' showguides='true' inkscape:guide-bbox='true' units='px'%3E %3Cinkscape:grid type='xygrid' id='grid6887' originx='-5.9568231' originy='-922.09998' /%3E %3Csodipodi:guide position='134.79086,343.16329' orientation='0,1' id='guide4803' /%3E %3C/sodipodi:namedview%3E %3Cmetadata id='metadata7'%3E %3Crdf:RDF%3E %3Ccc:Work rdf:about=''%3E %3Cdc:format%3Eimage/svg+xml%3C/dc:format%3E %3Cdc:type rdf:resource='http://purl.org/dc/dcmitype/StillImage' /%3E %3Cdc:title /%3E %3C/cc:Work%3E %3C/rdf:RDF%3E %3C/metadata%3E %3Cg inkscape:label='knob-background' inkscape:groupmode='layer' id='layer1' style='display:inline' transform='translate(-5.9568232,-35.262201)'%3E %3Cg id='g6790' transform='matrix(0.7404283,0,0,0.7404283,-16.505639,-495.70166)'%3E %3Ccircle transform='scale(-1,1)' r='63.63961' cy='781.26819' cx='-94.47673' id='path3338' style='fill:%23787e9f;fill-opacity:1;fill-rule:evenodd;stroke:%23787e9f;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:round;stroke-opacity:1' /%3E %3Crect transform='scale(1,-1)' y='-784.38281' x='91.783913' height='60.403229' width='5.4254837' id='rect4144' style='fill:%23ffffff;fill-opacity:1;stroke:%23000000;stroke-width:0.81103253;stroke-linejoin:round;stroke-opacity:0' /%3E %3Crect transform='matrix(0,-1,-1,0,0,0)' y='-95.268585' x='-785.54913' height='58.806583' width='1.5347334' id='rect4144-2' style='fill:%23ffffff;fill-opacity:1;stroke:%23000000;stroke-width:0.42561644;stroke-linejoin:round;stroke-opacity:0' /%3E %3Crect transform='matrix(0,1,1,0,0,0)' y='94.729378' x='784.02789' height='57.76585' width='1.5075723' id='rect4144-1' style='fill:%23ffffff;fill-opacity:1;stroke:%23000000;stroke-width:0.41808408;stroke-linejoin:round;stroke-opacity:0' /%3E %3Crect transform='matrix(0.5,-0.8660254,-0.8660254,-0.5,0,0)' y='-474.68332' x='-633.16095' height='58.438953' width='1.525139' id='rect4144-2-6' style='fill:%23ffffff;fill-opacity:1;stroke:%23000000;stroke-width:0.42295569;stroke-linejoin:round;stroke-opacity:0' /%3E %3Crect transform='matrix(0.5,0.8660254,0.8660254,-0.5,0,0)' y='-310.55344' x='726.5202' height='58.285332' width='1.5211297' id='rect4144-2-6-3' style='fill:%23ffffff;fill-opacity:1;stroke:%23000000;stroke-width:0.42184386;stroke-linejoin:round;stroke-opacity:0' /%3E %3Crect transform='matrix(0.8660254,0.5,0.5,-0.8660254,0,0)' y='-632.7262' x='473.62811' height='59.216183' width='1.5454232' id='rect4144-2-6-3-2' style='fill:%23ffffff;fill-opacity:1;stroke:%23000000;stroke-width:0.42858094;stroke-linejoin:round;stroke-opacity:0' /%3E %3Crect transform='matrix(0.8660254,-0.5,-0.5,-0.8660254,0,0)' y='-726.9054' x='-311.18314' height='58.014194' width='1.5140537' id='rect4144-2-6-9' style='fill:%23ffffff;fill-opacity:1;stroke:%23000000;stroke-width:0.41988149;stroke-linejoin:round;stroke-opacity:0' /%3E %3Crect transform='matrix(-0.96592583,-0.25881904,-0.5,0.8660254,0,0)' y='759.33832' x='-492.80673' height='57.483643' width='3.1062579' id='rect4144-2-6-7' style='fill:%23ffffff;fill-opacity:1;stroke:none;stroke-width:0.463;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0' /%3E %3Crect transform='matrix(0.70710677,-0.70710679,-0.5,-0.8660254,0,0)' y='-701.51379' x='-323.34305' height='57.938328' width='3.1308279' id='rect4144-2-6-7-8' style='fill:%23ffffff;fill-opacity:1;fill-rule:nonzero;stroke:%23000000;stroke-width:0.463;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0' /%3E %3Crect transform='matrix(-0.5,-0.8660254,-0.8660254,0.5,0,0)' y='310.76205' x='-727.69421' height='58.09866' width='1.5162581' id='rect4144-2-2' style='fill:%23ffffff;fill-opacity:1;stroke:%23000000;stroke-width:0.4204928;stroke-linejoin:round;stroke-opacity:0' /%3E %3Crect transform='matrix(0.5,-0.8660254,-0.8660254,-0.5,0,0)' y='-530.71777' x='-632.9516' height='55.885605' width='1.4585018' id='rect4144-2-2-0' style='fill:%23ffffff;fill-opacity:1;stroke:%23000000;stroke-width:0.40447566;stroke-linejoin:round;stroke-opacity:0' /%3E %3Crect transform='matrix(-0.96592583,-0.25881904,-0.5,0.8660254,0,0)' y='759.34033' x='-489.74704' height='57.483643' width='3.1062579' id='rect4144-2-6-7-3' style='display:inline;fill:%23ffffff;fill-opacity:1;stroke:none;stroke-width:0.463;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0' /%3E %3Crect transform='matrix(0.70710677,-0.70710679,-0.5,-0.8660254,0,0)' y='-701.51331' x='-320.27475' height='57.938328' width='3.1308279' id='rect4144-2-6-7-8-6' style='display:inline;fill:%23ffffff;fill-opacity:1;fill-rule:nonzero;stroke:%23000000;stroke-width:0.46299997;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0' /%3E %3C/g%3E %3C/g%3E %3Cg inkscape:groupmode='layer' id='layer2' inkscape:label='knob-foreground' style='display:inline' transform='translate(-5.9568232,-35.262201)' /%3E %3C/svg%3E\""

/***/ },
/* 21 */
/***/ function(module, exports) {

	module.exports = "\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8' standalone='no'?%3E %3C!-- Created with Inkscape (http://www.inkscape.org/) --%3E %3Csvg xmlns:osb='http://www.openswatchbook.org/uri/2009/osb' xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:cc='http://creativecommons.org/ns%23' xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns%23' xmlns:svg='http://www.w3.org/2000/svg' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd' xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape' width='95' height='95' viewBox='0 0 95.000001 95.000001' id='svg2' version='1.1' sodipodi:docname='fg.svg' inkscape:version='0.91 r13725'%3E %3Cdefs id='defs4'%3E %3ClinearGradient id='linearGradient6764' osb:paint='solid'%3E %3Cstop style='stop-color:%23000000;stop-opacity:1;' offset='0' id='stop6766' /%3E %3C/linearGradient%3E %3ClinearGradient id='linearGradient6758' osb:paint='solid'%3E %3Cstop style='stop-color:%23ffffff;stop-opacity:1;' offset='0' id='stop6760' /%3E %3C/linearGradient%3E %3ClinearGradient id='linearGradient6712' osb:paint='solid'%3E %3Cstop style='stop-color:%234d4d4d;stop-opacity:1;' offset='0' id='stop6714' /%3E %3C/linearGradient%3E %3ClinearGradient id='linearGradient6704' osb:paint='gradient'%3E %3Cstop style='stop-color:%23ffffff;stop-opacity:1;' offset='0' id='stop6706' /%3E %3Cstop style='stop-color:%23ffffff;stop-opacity:0;' offset='1' id='stop6708' /%3E %3C/linearGradient%3E %3ClinearGradient id='linearGradient6694' osb:paint='solid'%3E %3Cstop style='stop-color:%23ffffff;stop-opacity:1;' offset='0' id='stop6696' /%3E %3C/linearGradient%3E %3ClinearGradient id='linearGradient6680' osb:paint='gradient'%3E %3Cstop style='stop-color:%23ffffff;stop-opacity:1;' offset='0' id='stop6682' /%3E %3Cstop style='stop-color:%23ffffff;stop-opacity:0;' offset='1' id='stop6684' /%3E %3C/linearGradient%3E %3ClinearGradient id='linearGradient6568' osb:paint='solid'%3E %3Cstop style='stop-color:%23b5b5b5;stop-opacity:1;' offset='0' id='stop6570' /%3E %3C/linearGradient%3E %3Cfilter inkscape:menu-tooltip='Basic diffuse bevel to use for building textures' inkscape:menu='ABCs' inkscape:label='Diffuse light' id='f117' style='color-interpolation-filters:sRGB'%3E %3CfeGaussianBlur id='feGaussianBlur6484' result='result0' in='SourceGraphic' stdDeviation='6' /%3E %3CfeDiffuseLighting id='feDiffuseLighting6486' result='result5' surfaceScale='10' diffuseConstant='1'%3E %3CfeDistantLight id='feDistantLight6488' azimuth='235' elevation='25' /%3E %3C/feDiffuseLighting%3E %3CfeComposite id='feComposite6490' k3='0.6' k2='0' operator='arithmetic' result='result4' in='result5' in2='SourceGraphic' k1='1' k4='0' /%3E %3C/filter%3E %3Cpattern xlink:href='%23Strips1_1' id='pattern6806' patternTransform='matrix(30,0,0,10,-770,-710)' /%3E %3Cpattern id='Strips1_1' patternTransform='translate(0,0) scale(10,10)' height='1' width='2' patternUnits='userSpaceOnUse'%3E %3Crect id='rect5384' height='2' width='1' y='-0.5' x='0' /%3E %3C/pattern%3E %3C/defs%3E %3Csodipodi:namedview id='base' pagecolor='%237b7b7b' bordercolor='%23666666' borderopacity='1.0' inkscape:pageopacity='0' inkscape:pageshadow='2' inkscape:zoom='2.8284271' inkscape:cx='41.258451' inkscape:cy='54.1403' inkscape:document-units='px' inkscape:current-layer='g6790' showgrid='false' inkscape:snap-object-midpoints='false' inkscape:snap-others='true' inkscape:snap-center='false' inkscape:snap-nodes='true' inkscape:snap-grids='true' inkscape:snap-global='true' inkscape:window-width='1366' inkscape:window-height='716' inkscape:window-x='316' inkscape:window-y='1080' inkscape:window-maximized='1' showborder='true' borderlayer='false' fit-margin-top='0' fit-margin-left='0' fit-margin-right='0' fit-margin-bottom='0' showguides='true' inkscape:guide-bbox='true' units='px'%3E %3Cinkscape:grid type='xygrid' id='grid6887' originx='-5.9568231' originy='-922.09998' /%3E %3Csodipodi:guide position='134.79086,343.16329' orientation='0,1' id='guide4803' /%3E %3C/sodipodi:namedview%3E %3Cmetadata id='metadata7'%3E %3Crdf:RDF%3E %3Ccc:Work rdf:about=''%3E %3Cdc:format%3Eimage/svg+xml%3C/dc:format%3E %3Cdc:type rdf:resource='http://purl.org/dc/dcmitype/StillImage' /%3E %3Cdc:title%3E%3C/dc:title%3E %3C/cc:Work%3E %3C/rdf:RDF%3E %3C/metadata%3E %3Cg inkscape:label='knob-background' inkscape:groupmode='layer' id='layer1' style='display:inline' transform='translate(-5.9568232,-35.262201)'%3E %3Cg id='g6790' transform='matrix(0.7404283,0,0,0.7404283,-16.505639,-495.70166)'%3E %3Cg id='g3595' transform='translate(0,-1.5118408)'%3E %3Ccircle style='fill:%23484848;stroke:%23484848;stroke-width:0.98509628' id='circle4202' cx='94.47673' cy='782.78003' r='44.329334' /%3E %3Ccircle style='fill:%233d3d3d;stroke:%23484848;stroke-width:0.49254814' id='circle4204' cx='94.47673' cy='782.78003' r='34.478371' /%3E %3Ccircle style='fill:%23484848;stroke:%23484848;stroke-width:0.98509628' id='circle4206' cx='94.47673' cy='782.78003' r='19.701925' /%3E %3C/g%3E %3Cg transform='matrix(0.98050088,0,0,1.0313289,-103.09215,719.17998)' id='g4208'%3E %3Cpath d='m 197.01675,13.185169 c -0.8127,17.013009 -1.62546,34.026017 -2.43818,51.039026 4.91187,-0.0059 9.82378,-0.01317 14.73565,-0.0198 -0.76953,-17.015837 -1.5391,-34.031684 -2.30864,-51.047521 -3.32961,0.0098 -6.6592,0.01892 -9.98883,0.02828 z' id='path4210' inkscape:connector-curvature='0' style='fill:%23e2e2e2;stroke:url(%23pattern6806);stroke-width:3.44300008;stroke-opacity:0.18999999' /%3E %3C/g%3E %3C/g%3E %3C/g%3E %3Cg inkscape:groupmode='layer' id='layer2' inkscape:label='knob-foreground' style='display:inline' transform='translate(-5.9568232,-35.262201)' /%3E %3C/svg%3E\""

/***/ },
/* 22 */
/***/ function(module, exports) {

	module.exports = "\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8' standalone='no'?%3E %3C!-- Created with Inkscape (http://www.inkscape.org/) --%3E %3Csvg xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:cc='http://creativecommons.org/ns%23' xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns%23' xmlns:svg='http://www.w3.org/2000/svg' xmlns='http://www.w3.org/2000/svg' xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd' xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape' width='9.0351591mm' height='4.8058167mm' viewBox='0 0 32.014343 17.028484' id='svg2' version='1.1' inkscape:version='0.91 r13725' sodipodi:docname='pulse.svg'%3E %3Cdefs id='defs4' /%3E %3Csodipodi:namedview id='base' pagecolor='%23767676' bordercolor='%23ffffff' borderopacity='1' inkscape:pageopacity='0' inkscape:pageshadow='2' inkscape:zoom='2.8284273' inkscape:cx='-142.2296' inkscape:cy='-59.472086' inkscape:document-units='px' inkscape:current-layer='layer1' showgrid='true' borderlayer='true' inkscape:showpageshadow='true' inkscape:window-width='1920' inkscape:window-height='1001' inkscape:window-x='0' inkscape:window-y='27' inkscape:window-maximized='1' inkscape:snap-grids='true' fit-margin-top='0' fit-margin-left='0' fit-margin-right='0' fit-margin-bottom='0'%3E %3Cinkscape:grid type='xygrid' id='grid3336' color='%23b4bc45' opacity='0.1254902' originx='-18.49985' originy='-1998.4786' /%3E %3C/sodipodi:namedview%3E %3Cmetadata id='metadata7'%3E %3Crdf:RDF%3E %3Ccc:Work rdf:about=''%3E %3Cdc:format%3Eimage/svg+xml%3C/dc:format%3E %3Cdc:type rdf:resource='http://purl.org/dc/dcmitype/StillImage' /%3E %3Cdc:title /%3E %3C/cc:Work%3E %3C/rdf:RDF%3E %3C/metadata%3E %3Cg inkscape:label='Layer 1' inkscape:groupmode='layer' id='layer1' transform='translate(-8.9999,-36.347963)'%3E %3Cpath style='opacity:1;fill:%23ffffff;fill-opacity:1;fill-rule:nonzero;stroke:%234d4d4d;stroke-width:0.50431865;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0.19444448' d='' id='path3348' inkscape:connector-curvature='0' /%3E %3Cg id='g4164' style='stroke-width:1.5;stroke-miterlimit:4;stroke-dasharray:none'%3E %3Cpath inkscape:connector-curvature='0' id='path4158' d='m 10,52.362205 0,-15 10,0' style='fill:none;fill-rule:evenodd;stroke:%23ffffff;stroke-width:1.5;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' /%3E %3Cpath inkscape:connector-curvature='0' id='path4160' d='m 20,37.362205 0,15 20,0' style='fill:none;fill-rule:evenodd;stroke:%23ffffff;stroke-width:1.5;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' /%3E %3C/g%3E %3C/g%3E %3C/svg%3E\""

/***/ },
/* 23 */
/***/ function(module, exports) {

	module.exports = "\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8' standalone='no'?%3E %3C!-- Created with Inkscape (http://www.inkscape.org/) --%3E %3Csvg xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:cc='http://creativecommons.org/ns%23' xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns%23' xmlns:svg='http://www.w3.org/2000/svg' xmlns='http://www.w3.org/2000/svg' xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd' xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape' width='25.578321' height='12.02346' viewBox='0 0 25.578321 12.02346' id='svg2' version='1.1' inkscape:version='0.91 r13725' sodipodi:docname='release.svg'%3E %3Cdefs id='defs4' /%3E %3Csodipodi:namedview id='base' pagecolor='%23767676' bordercolor='%23ffffff' borderopacity='1' inkscape:pageopacity='0' inkscape:pageshadow='2' inkscape:zoom='22.627418' inkscape:cx='15.427557' inkscape:cy='10.202857' inkscape:document-units='px' inkscape:current-layer='layer1' showgrid='true' borderlayer='true' inkscape:showpageshadow='true' inkscape:window-width='1920' inkscape:window-height='1001' inkscape:window-x='0' inkscape:window-y='27' inkscape:window-maximized='1' inkscape:snap-grids='true' fit-margin-top='0' fit-margin-left='0' fit-margin-right='0' fit-margin-bottom='0' units='px'%3E %3Cinkscape:grid type='xygrid' id='grid3336' color='%23b4bc45' opacity='0.1254902' originx='-122.9929' originy='-2128.9875' /%3E %3C/sodipodi:namedview%3E %3Cmetadata id='metadata7'%3E %3Crdf:RDF%3E %3Ccc:Work rdf:about=''%3E %3Cdc:format%3Eimage/svg+xml%3C/dc:format%3E %3Cdc:type rdf:resource='http://purl.org/dc/dcmitype/StillImage' /%3E %3Cdc:title%3E%3C/dc:title%3E %3C/cc:Work%3E %3C/rdf:RDF%3E %3C/metadata%3E %3Cg inkscape:label='Layer 1' inkscape:groupmode='layer' id='layer1' transform='translate(-113.49295,89.155926)'%3E %3Cpath style='opacity:1;fill:%23ffffff;fill-opacity:1;fill-rule:nonzero;stroke:%234d4d4d;stroke-width:0.50431865;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0.19444448' d='' id='path3348' inkscape:connector-curvature='0' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23787e9f;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 114.50005,-78.144855 c 2.03357,-3.570745 4.12089,-7.121335 8,-10 l 0,0' id='path4306' inkscape:connector-curvature='0' sodipodi:nodetypes='ccc' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23787e9f;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 122.50005,-88.144855 c 1.02705,1.26253 2.15887,2.435254 4,3 l 0,0' id='path4308' inkscape:connector-curvature='0' sodipodi:nodetypes='ccc' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23787e9f;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 126.50005,-85.144855 7,0' id='path4979' inkscape:connector-curvature='0' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23ffffff;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 133.56118,-85.083165 c 0.86637,2.733835 1.81205,5.405611 4.49585,6.717115 l 0,0' id='path4983' inkscape:connector-curvature='0' sodipodi:nodetypes='ccc' /%3E %3C/g%3E %3C/svg%3E\""

/***/ },
/* 24 */
/***/ function(module, exports) {

	module.exports = "\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8' standalone='no'?%3E %3C!-- Created with Inkscape (http://www.inkscape.org/) --%3E %3Csvg xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:cc='http://creativecommons.org/ns%23' xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns%23' xmlns:svg='http://www.w3.org/2000/svg' xmlns='http://www.w3.org/2000/svg' xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd' xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape' width='9.0387287mm' height='4.8058171mm' viewBox='0 0 32.026992 17.028485' id='svg2' version='1.1' inkscape:version='0.91 r13725' sodipodi:docname='saw.svg'%3E %3Cdefs id='defs4' /%3E %3Csodipodi:namedview id='base' pagecolor='%23767676' bordercolor='%23ffffff' borderopacity='1' inkscape:pageopacity='0' inkscape:pageshadow='2' inkscape:zoom='11.313709' inkscape:cx='-23.76401' inkscape:cy='-3.3776791' inkscape:document-units='px' inkscape:current-layer='layer1' showgrid='true' borderlayer='true' inkscape:showpageshadow='true' inkscape:window-width='1920' inkscape:window-height='1001' inkscape:window-x='0' inkscape:window-y='27' inkscape:window-maximized='1' inkscape:snap-grids='true' fit-margin-top='0' fit-margin-left='0' fit-margin-right='0' fit-margin-bottom='0'%3E %3Cinkscape:grid type='xygrid' id='grid3336' color='%23b4bc45' opacity='0.1254902' originx='37.012848' originy='-1997.9714' /%3E %3C/sodipodi:namedview%3E %3Cmetadata id='metadata7'%3E %3Crdf:RDF%3E %3Ccc:Work rdf:about=''%3E %3Cdc:format%3Eimage/svg+xml%3C/dc:format%3E %3Cdc:type rdf:resource='http://purl.org/dc/dcmitype/StillImage' /%3E %3Cdc:title /%3E %3C/cc:Work%3E %3C/rdf:RDF%3E %3C/metadata%3E %3Cg inkscape:label='Layer 1' inkscape:groupmode='layer' id='layer1' transform='translate(46.512799,-36.855105)'%3E %3Cpath style='opacity:1;fill:%23ffffff;fill-opacity:1;fill-rule:nonzero;stroke:%234d4d4d;stroke-width:0.50431865;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0.19444448' d='' id='path3348' inkscape:connector-curvature='0' /%3E %3Cg transform='translate(-95.50005,30.507143)' id='g4172' style='stroke-width:1.5;stroke-miterlimit:4;stroke-dasharray:none'%3E %3Cpath inkscape:connector-curvature='0' id='path4152' d='m 50,22.362205 30,-15.0000003 0,0' style='fill:none;fill-rule:evenodd;stroke:%23ffffff;stroke-width:1.5;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' /%3E %3Cpath inkscape:connector-curvature='0' id='path4154' d='M 80,7.3622047 80,22.362205' style='fill:none;fill-rule:evenodd;stroke:%23ffffff;stroke-width:1.5;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' /%3E %3C/g%3E %3C/g%3E %3C/svg%3E\""

/***/ },
/* 25 */
/***/ function(module, exports) {

	module.exports = "\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8' standalone='no'?%3E %3C!-- Created with Inkscape (http://www.inkscape.org/) --%3E %3Csvg xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:cc='http://creativecommons.org/ns%23' xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns%23' xmlns:svg='http://www.w3.org/2000/svg' xmlns='http://www.w3.org/2000/svg' xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd' xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape' width='30.656002' height='14.824542' viewBox='0 0 30.656002 14.824541' id='svg2' version='1.1' inkscape:version='0.91 r13725' sodipodi:docname='sine.svg'%3E %3Cdefs id='defs4' /%3E %3Csodipodi:namedview id='base' pagecolor='%23767676' bordercolor='%23ffffff' borderopacity='1' inkscape:pageopacity='0' inkscape:pageshadow='2' inkscape:zoom='11.313709' inkscape:cx='61.382421' inkscape:cy='16.869443' inkscape:document-units='px' inkscape:current-layer='layer1' showgrid='true' borderlayer='true' inkscape:showpageshadow='true' inkscape:window-width='1920' inkscape:window-height='1001' inkscape:window-x='0' inkscape:window-y='27' inkscape:window-maximized='1' inkscape:snap-grids='true' fit-margin-top='0' fit-margin-left='0' fit-margin-right='0' fit-margin-bottom='0' units='px'%3E %3Cinkscape:grid type='xygrid' id='grid3336' color='%23b4bc45' opacity='0.1254902' originx='-49.644373' originy='-1000.0742' dotted='false' /%3E %3C/sodipodi:namedview%3E %3Cmetadata id='metadata7'%3E %3Crdf:RDF%3E %3Ccc:Work rdf:about=''%3E %3Cdc:format%3Eimage/svg+xml%3C/dc:format%3E %3Cdc:type rdf:resource='http://purl.org/dc/dcmitype/StillImage' /%3E %3Cdc:title /%3E %3C/cc:Work%3E %3C/rdf:RDF%3E %3C/metadata%3E %3Cg inkscape:label='Layer 1' inkscape:groupmode='layer' id='layer1' transform='translate(-49.644373,-37.463506)'%3E %3Cpath style='opacity:1;fill:%23ffffff;fill-opacity:1;fill-rule:nonzero;stroke:%234d4d4d;stroke-width:0.50431865;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0.19444448' d='' id='path3348' inkscape:connector-curvature='0' /%3E %3Cg id='g4183' style='stroke:%23ffffff;stroke-linecap:round;stroke-linejoin:round;stroke-opacity:1;stroke-width:1.5;stroke-miterlimit:4;stroke-dasharray:none'%3E %3Cpath sodipodi:nodetypes='cc' inkscape:connector-curvature='0' id='path4156' d='m 50,44.880464 c 4.400662,-8.279519 9.243899,-10.493239 14.973982,0' style='fill:none;fill-rule:evenodd;stroke:%23ffffff;stroke-width:1.5;stroke-linecap:round;stroke-linejoin:round;stroke-opacity:1;stroke-miterlimit:4;stroke-dasharray:none' /%3E %3Cpath sodipodi:nodetypes='cc' inkscape:connector-curvature='0' id='path4156-6' d='m 79.944748,44.871088 c -4.400662,8.279519 -9.243899,10.493239 -14.973983,0' style='fill:none;fill-rule:evenodd;stroke:%23ffffff;stroke-width:1.5;stroke-linecap:round;stroke-linejoin:round;stroke-opacity:1;stroke-miterlimit:4;stroke-dasharray:none' /%3E %3C/g%3E %3C/g%3E %3C/svg%3E\""

/***/ },
/* 26 */
/***/ function(module, exports) {

	module.exports = "\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8' standalone='no'?%3E %3C!-- Created with Inkscape (http://www.inkscape.org/) --%3E %3Csvg xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:cc='http://creativecommons.org/ns%23' xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns%23' xmlns:svg='http://www.w3.org/2000/svg' xmlns='http://www.w3.org/2000/svg' xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd' xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape' width='31.215384' height='15.024919' viewBox='0 0 31.215383 15.024917' id='svg2' version='1.1' inkscape:version='0.91 r13725' sodipodi:docname='square.svg'%3E %3Cdefs id='defs4' /%3E %3Csodipodi:namedview id='base' pagecolor='%23767676' bordercolor='%23ffffff' borderopacity='1' inkscape:pageopacity='0' inkscape:pageshadow='2' inkscape:zoom='8.0000003' inkscape:cx='37.200259' inkscape:cy='-1.2929871' inkscape:document-units='px' inkscape:current-layer='layer1' showgrid='true' borderlayer='true' inkscape:showpageshadow='true' inkscape:window-width='1920' inkscape:window-height='1001' inkscape:window-x='0' inkscape:window-y='27' inkscape:window-maximized='1' inkscape:snap-grids='true' fit-margin-top='0' fit-margin-left='0' fit-margin-right='0' fit-margin-bottom='0' units='px'%3E %3Cinkscape:grid type='xygrid' id='grid3336' color='%23b4bc45' opacity='0.1254902' originx='-49.282932' originy='-999.98744' dotted='false' /%3E %3C/sodipodi:namedview%3E %3Cmetadata id='metadata7'%3E %3Crdf:RDF%3E %3Ccc:Work rdf:about=''%3E %3Cdc:format%3Eimage/svg+xml%3C/dc:format%3E %3Cdc:type rdf:resource='http://purl.org/dc/dcmitype/StillImage' /%3E %3Cdc:title /%3E %3C/cc:Work%3E %3C/rdf:RDF%3E %3C/metadata%3E %3Cg inkscape:label='Layer 1' inkscape:groupmode='layer' id='layer1' transform='translate(-49.282933,-37.34986)'%3E %3Cpath style='opacity:1;fill:%23ffffff;fill-opacity:1;fill-rule:nonzero;stroke:%234d4d4d;stroke-width:0.50431865;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0.19444448' d='' id='path3348' inkscape:connector-curvature='0' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23ffffff;stroke-width:1.5;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 50.283033,51.360536 0,-12.996434 15.111302,0 0,12.996434 14.103882,0 0,-12.996434' id='path6229' inkscape:connector-curvature='0' /%3E %3C/g%3E %3C/svg%3E\""

/***/ },
/* 27 */
/***/ function(module, exports) {

	module.exports = "\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8' standalone='no'?%3E %3C!-- Created with Inkscape (http://www.inkscape.org/) --%3E %3Csvg xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:cc='http://creativecommons.org/ns%23' xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns%23' xmlns:svg='http://www.w3.org/2000/svg' xmlns='http://www.w3.org/2000/svg' xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd' xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape' width='25.578321' height='12.02346' viewBox='0 0 25.578321 12.02346' id='svg2' version='1.1' inkscape:version='0.91 r13725' sodipodi:docname='sustain.svg'%3E %3Cdefs id='defs4' /%3E %3Csodipodi:namedview id='base' pagecolor='%23767676' bordercolor='%23ffffff' borderopacity='1' inkscape:pageopacity='0' inkscape:pageshadow='2' inkscape:zoom='22.627418' inkscape:cx='15.427557' inkscape:cy='10.202857' inkscape:document-units='px' inkscape:current-layer='layer1' showgrid='true' borderlayer='true' inkscape:showpageshadow='true' inkscape:window-width='1920' inkscape:window-height='1001' inkscape:window-x='0' inkscape:window-y='27' inkscape:window-maximized='1' inkscape:snap-grids='true' fit-margin-top='0' fit-margin-left='0' fit-margin-right='0' fit-margin-bottom='0' units='px'%3E %3Cinkscape:grid type='xygrid' id='grid3336' color='%23b4bc45' opacity='0.1254902' originx='-122.9929' originy='-2128.9875' /%3E %3C/sodipodi:namedview%3E %3Cmetadata id='metadata7'%3E %3Crdf:RDF%3E %3Ccc:Work rdf:about=''%3E %3Cdc:format%3Eimage/svg+xml%3C/dc:format%3E %3Cdc:type rdf:resource='http://purl.org/dc/dcmitype/StillImage' /%3E %3Cdc:title%3E%3C/dc:title%3E %3C/cc:Work%3E %3C/rdf:RDF%3E %3C/metadata%3E %3Cg inkscape:label='Layer 1' inkscape:groupmode='layer' id='layer1' transform='translate(-113.49295,89.155926)'%3E %3Cpath style='opacity:1;fill:%23ffffff;fill-opacity:1;fill-rule:nonzero;stroke:%234d4d4d;stroke-width:0.50431865;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0.19444448' d='' id='path3348' inkscape:connector-curvature='0' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23787e9f;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 114.50005,-78.144855 c 2.03357,-3.570745 4.12089,-7.121335 8,-10 l 0,0' id='path4306' inkscape:connector-curvature='0' sodipodi:nodetypes='ccc' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23787e9f;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 122.50005,-88.144855 c 1.02705,1.26253 2.15887,2.435254 4,3 l 0,0' id='path4308' inkscape:connector-curvature='0' sodipodi:nodetypes='ccc' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23787e9f;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 133.56118,-85.083165 c 0.86637,2.733835 1.81205,5.405611 4.49585,6.717115 l 0,0' id='path4983' inkscape:connector-curvature='0' sodipodi:nodetypes='ccc' /%3E %3Cpath style='fill:none;fill-rule:evenodd;stroke:%23ffffff;stroke-width:2;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 126.50005,-85.144855 7,0' id='path4979' inkscape:connector-curvature='0' /%3E %3C/g%3E %3C/svg%3E\""

/***/ },
/* 28 */
/***/ function(module, exports) {

	module.exports = "\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8' standalone='no'?%3E %3C!-- Created with Inkscape (http://www.inkscape.org/) --%3E %3Csvg xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:cc='http://creativecommons.org/ns%23' xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns%23' xmlns:svg='http://www.w3.org/2000/svg' xmlns='http://www.w3.org/2000/svg' xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd' xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape' width='9.1072025mm' height='3.8863542mm' viewBox='0 0 32.269615 13.770546' id='svg2' version='1.1' inkscape:version='0.91 r13725' sodipodi:docname='triangle.svg'%3E %3Cdefs id='defs4' /%3E %3Csodipodi:namedview id='base' pagecolor='%23767676' bordercolor='%23ffffff' borderopacity='1' inkscape:pageopacity='0' inkscape:pageshadow='2' inkscape:zoom='14.882794' inkscape:cx='2.2292639' inkscape:cy='7.4407221' inkscape:document-units='px' inkscape:current-layer='g4145' showgrid='true' borderlayer='true' inkscape:showpageshadow='true' inkscape:window-width='1920' inkscape:window-height='1001' inkscape:window-x='0' inkscape:window-y='27' inkscape:window-maximized='1' inkscape:snap-grids='true' fit-margin-top='0' fit-margin-left='0' fit-margin-right='0' fit-margin-bottom='0'%3E %3Cinkscape:grid type='xygrid' id='grid3336' color='%23b4bc45' opacity='0.1254902' originx='-8.8651931' originy='-1028.8677' /%3E %3C/sodipodi:namedview%3E %3Cmetadata id='metadata7'%3E %3Crdf:RDF%3E %3Ccc:Work rdf:about=''%3E %3Cdc:format%3Eimage/svg+xml%3C/dc:format%3E %3Cdc:type rdf:resource='http://purl.org/dc/dcmitype/StillImage' /%3E %3Cdc:title /%3E %3C/cc:Work%3E %3C/rdf:RDF%3E %3C/metadata%3E %3Cg inkscape:label='Layer 1' inkscape:groupmode='layer' id='layer1' transform='translate(-8.8651932,-9.7239158)'%3E %3Cg id='g4145'%3E %3Cg id='g4149' style='stroke-width:1.5;stroke-miterlimit:4;stroke-dasharray:none'%3E %3Cpath style='fill:none;fill-opacity:1;fill-rule:evenodd;stroke:%23ffffff;stroke-width:1.5;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0.89570552' d='M 9.8764601,22.485684 C 25,10.732694 25,10.732694 25,10.732694' id='path3344' inkscape:connector-curvature='0' /%3E %3Cpath style='fill:none;fill-opacity:1;fill-rule:evenodd;stroke:%23ffffff;stroke-width:1.5;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0.89570552' d='m 25,10.732694 15.12354,11.75299' id='path3346' inkscape:connector-curvature='0' /%3E %3C/g%3E %3C/g%3E %3Cpath style='opacity:1;fill:%23ffffff;fill-opacity:1;fill-rule:nonzero;stroke:%234d4d4d;stroke-width:0.50431865;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:0.19444448' d='' id='path3348' inkscape:connector-curvature='0' /%3E %3C/g%3E %3C/svg%3E\""

/***/ },
/* 29 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";
	const Synth_1 = __webpack_require__(36);
	const MIDI_1 = __webpack_require__(2);
	const PresetManager_1 = __webpack_require__(37);
	const Elm = __webpack_require__(11);
	const presets = __webpack_require__(14);
	const scaleMidiValue = (midiValue) => MIDI_1.default.logScaleToMax(midiValue, 1);
	const noMidiMsg = `Your browser doesnt support WebMIDI API. Use another
		browser or install the Jazz Midi Plugin http://jazz-soft.net/`;
	const midiSettingsToSynthSettings = (preset) => {
	    let state = {
	        amp: { adsr: {} },
	        filter: { adsr: {} },
	        oscs: { osc1: {}, osc2: {} }
	    };
	    /* META */
	    //displayed name
	    state.name = preset.name;
	    /* AMP */
	    state.amp.adsr.attack =
	        scaleMidiValue(preset.amp.adsr.attack);
	    state.amp.adsr.decay =
	        scaleMidiValue(preset.amp.adsr.decay);
	    state.amp.adsr.sustain =
	        scaleMidiValue(preset.amp.adsr.sustain);
	    state.amp.adsr.release =
	        scaleMidiValue(preset.amp.adsr.release);
	    state.amp.masterVolume =
	        scaleMidiValue(preset.amp.masterVolume);
	    /* OVERDRIVE */
	    state.overdrive =
	        preset.overdrive;
	    /* FILTER */
	    state.filter.type_ =
	        preset.filter.type_;
	    state.filter.envelopeAmount =
	        scaleMidiValue(preset.filter.envelopeAmount);
	    state.filter.q =
	        MIDI_1.default.toFilterQAmount(preset.filter.q);
	    state.filter.frequency =
	        MIDI_1.default.toFilterCutoffFrequency(preset.filter.frequency);
	    state.filter.adsr.attack =
	        scaleMidiValue(preset.filter.adsr.attack);
	    state.filter.adsr.decay =
	        scaleMidiValue(preset.filter.adsr.decay);
	    state.filter.adsr.sustain =
	        scaleMidiValue(preset.filter.adsr.sustain);
	    state.filter.adsr.release =
	        scaleMidiValue(preset.filter.adsr.release);
	    /* OSC */
	    state.oscs.pw =
	        MIDI_1.default.logScaleToMax(preset.oscs.pw, .9);
	    state.oscs.mix =
	        MIDI_1.default.normalizeValue(preset.oscs.mix);
	    state.oscs.osc1.waveformType =
	        preset.oscs.osc1.waveformType;
	    state.oscs.osc1.fmGain =
	        scaleMidiValue(preset.oscs.osc1.fmGain);
	    state.oscs.osc2.waveformType =
	        preset.oscs.osc2.waveformType;
	    state.oscs.osc2.semitone =
	        preset.oscs.osc2.semitone;
	    state.oscs.osc2.detune =
	        preset.oscs.osc2.detune;
	    state.oscs.osc2.kbdTrack =
	        preset.oscs.osc2.kbdTrack;
	    return state;
	};
	class Application {
	    constructor() {
	        this.nextPreset = () => {
	            const nextPreset = this.presetManager.next();
	            const synthState = midiSettingsToSynthSettings(nextPreset);
	            this.synth.setState(synthState);
	            this.app.ports.presetChange.send(nextPreset);
	        };
	        this.previousPreset = () => {
	            const previousPreset = this.presetManager.previous();
	            const synthState = midiSettingsToSynthSettings(previousPreset);
	            this.synth.setState(synthState);
	            this.app.ports.presetChange.send(previousPreset);
	        };
	        this.initializeSynth = () => {
	            this.presetManager = new PresetManager_1.default(presets);
	            const preset = this.presetManager.next();
	            const midiSupport = this.midiAccess ? true : false;
	            this.app = Elm.Main.fullscreen({
	                preset: preset,
	                midiSupport: midiSupport
	            });
	            const synthSettings = midiSettingsToSynthSettings(preset);
	            this.synth = new Synth_1.default(synthSettings);
	            // MACRO
	            window.onblur = () => {
	                this.app.ports.panic.send();
	                this.synth.oscillators.panic();
	            };
	            window.oncontextmenu = () => false;
	            this.app.ports.previousPreset
	                .subscribe(() => {
	                this.previousPreset();
	            });
	            this.app.ports.nextPreset
	                .subscribe(() => {
	                this.nextPreset();
	            });
	            // AMP
	            this.app.ports.ampVolume
	                .subscribe(this.synth.amplifier.setMasterVolumeGain);
	            this.app.ports.ampAttack
	                .subscribe(this.synth.amplifier.adsr.setAttack);
	            this.app.ports.ampDecay
	                .subscribe(this.synth.amplifier.adsr.setDecay);
	            this.app.ports.ampSustain
	                .subscribe(this.synth.amplifier.adsr.setSustain);
	            this.app.ports.ampRelease
	                .subscribe(this.synth.amplifier.adsr.setRelease);
	            // OSCILLATORS
	            this.app.ports.oscsBalance
	                .subscribe(this.synth.oscillators.setMix);
	            this.app.ports.osc2Semitone
	                .subscribe(this.synth.oscillators.setOscillator2Semitone);
	            this.app.ports.osc2Detune
	                .subscribe(this.synth.oscillators.setOscillator2Detune);
	            this.app.ports.fmAmount
	                .subscribe(this.synth.oscillators.setFmAmount);
	            this.app.ports.pulseWidth
	                .subscribe(this.synth.oscillators.setPulseWidth);
	            this.app.ports.osc1Waveform
	                .subscribe(this.synth.oscillators.setOscillator1Waveform);
	            this.app.ports.osc2Waveform
	                .subscribe(this.synth.oscillators.setOscillator2Waveform);
	            this.app.ports.osc2KbdTrack
	                .subscribe(this.synth.oscillators.toggleOsc2KbdTrack);
	            // FILTER
	            this.app.ports.filterEnvelopeAmount
	                .subscribe(this.synth.filter.setEnvelopeAmount);
	            this.app.ports.filterAttack
	                .subscribe(this.synth.filter.adsr.setAttack);
	            this.app.ports.filterDecay
	                .subscribe(this.synth.filter.adsr.setDecay);
	            this.app.ports.filterSustain
	                .subscribe(this.synth.filter.adsr.setSustain);
	            this.app.ports.filterRelease
	                .subscribe(this.synth.filter.adsr.setRelease);
	            this.app.ports.filterCutoff
	                .subscribe(this.synth.filter.setCutoff);
	            this.app.ports.filterQ
	                .subscribe(this.synth.filter.setQ);
	            this.app.ports.filterType
	                .subscribe(this.synth.filter.setType);
	            this.app.ports.overdrive
	                .subscribe(this.synth.overdrive.setState);
	            // MIDI
	            if (this.midiAccess) {
	                MIDI_1.default.manageMidiDevices(this.synth.onMIDIMessage, this.midiAccess, this.app.ports.midiIn, this.app.ports.midiStateChange);
	            }
	            this.app.ports.midiOut.subscribe(this.synth.onMIDIMessage);
	        };
	        const onMIDISuccess = (midiAccess) => {
	            this.midiAccess = midiAccess;
	        };
	        if (navigator.requestMIDIAccess) {
	            navigator
	                .requestMIDIAccess({ sysex: false })
	                .then(onMIDISuccess, () => alert(noMidiMsg))
	                .then(this.initializeSynth);
	        }
	        else {
	            alert(noMidiMsg);
	        }
	    }
	}
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.default = Application;


/***/ },
/* 30 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";
	const ADSR_1 = __webpack_require__(6);
	const MIDI_1 = __webpack_require__(2);
	class Amplifier {
	    constructor(context, state) {
	        this.state = {};
	        this.setMasterVolumeGain = (midiValue) => {
	            const vol = MIDI_1.default.logScaleToMax(midiValue, 1);
	            this.state.masterVolume = vol;
	            this.output.gain.value = vol;
	        };
	        this._setState = (state) => {
	            this.output.gain.value = state.masterVolume;
	            this.state.masterVolume = state.masterVolume;
	        };
	        this.setState = (state) => {
	            this.adsr.setState(state.adsr);
	            this._setState(state);
	        };
	        this.context = context;
	        /* amp adsr state */
	        this.adsr = new ADSR_1.ADSR(this.context, state.adsr);
	        /* AudioNode graph routing */
	        this.output = this.context.createGain();
	        this.output.connect(this.context.destination);
	        this._setState(state);
	    }
	}
	exports.Amplifier = Amplifier;


/***/ },
/* 31 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";
	const MIDI_1 = __webpack_require__(2);
	const Constants_1 = __webpack_require__(1);
	const ADSR_1 = __webpack_require__(6);
	class Filter {
	    constructor(context, state) {
	        this._setState = (state) => {
	            /* filter state */
	            this.biquadFilter.type = state.type_;
	            this.biquadFilter.frequency.value = state.frequency;
	            this.biquadFilter.Q.value = state.q;
	            this.envelopeAmount = state.envelopeAmount;
	        };
	        this.setState = (state) => {
	            this._setState(state);
	            this.adsr.setState(state.adsr);
	        };
	        /* triggers filter's attack, decay, and sustain envelope  */
	        this.noteOn = () => {
	            const filterMinFreq = this.biquadFilter.frequency.value;
	            let filterMaxFreq = this.envelopeAmount * MIDI_1.default.toFilterCutoffFrequency(127);
	            if (filterMaxFreq < filterMinFreq) {
	                filterMaxFreq = filterMinFreq;
	            }
	            const filterMaxInCents = 1200 * Math.log2(filterMaxFreq / filterMinFreq);
	            this.adsr.on(0, filterMaxInCents)(this.biquadFilter.detune);
	        };
	        /* triggers filter's release envelope  */
	        this.noteOff = () => {
	            this.adsr.off(this.biquadFilter.detune);
	        };
	        this.connect = (node) => {
	            this.output.connect(node);
	            return this;
	        };
	        this.disconnect = (node) => {
	            this.output.disconnect(node);
	            return this;
	        };
	        this.setCutoff = (midiValue) => {
	            this.biquadFilter.frequency.value =
	                MIDI_1.default.toFilterCutoffFrequency(midiValue);
	        };
	        this.setQ = (midiValue) => {
	            this.biquadFilter.Q.value = MIDI_1.default.toFilterQAmount(midiValue);
	        };
	        this.setType = (filterType) => {
	            const ft = filterType.toLowerCase();
	            if (Constants_1.default.FILTER_TYPES.indexOf(ft) !== -1) {
	                this.biquadFilter.type = ft;
	            }
	            else {
	                throw new Error('Invalid Filter Type');
	            }
	        };
	        this.setEnvelopeAmount = (midiValue) => {
	            this.envelopeAmount = MIDI_1.default.logScaleToMax(midiValue, 1);
	        };
	        this.context = context;
	        this.biquadFilter = this.context.createBiquadFilter();
	        /* AudioNode graph routing */
	        this.input = this.context.createGain();
	        this.output = this.context.createGain();
	        this.input.connect(this.biquadFilter);
	        this.biquadFilter.connect(this.output);
	        this.adsr = new ADSR_1.ADSR(this.context, state.adsr);
	        this._setState(state);
	    }
	    get type() {
	        return this.biquadFilter.type;
	    }
	}
	exports.Filter = Filter;


/***/ },
/* 32 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";
	const BaseOscillator_1 = __webpack_require__(7);
	class NoiseOscillator extends BaseOscillator_1.BaseOscillator {
	    constructor(context) {
	        super(context, 'whitenoise');
	    }
	    _noteOn(midiNote) {
	        const midiNoteKey = midiNote.toString();
	        const channels = 2;
	        const frameCount = this.context.sampleRate * 2.0;
	        const myArrayBuffer = this.context.createBuffer(2, frameCount, this.context.sampleRate);
	        for (let channel = 0; channel < channels; channel++) {
	            const nowBuffering = myArrayBuffer.getChannelData(channel);
	            for (let i = 0; i < frameCount; i++) {
	                nowBuffering[i] = Math.random() * 2 - 1;
	            }
	        }
	        this.noiseOsc = this.context.createBufferSource();
	        this.noiseOsc.buffer = myArrayBuffer;
	        this.noiseOsc.loop = true;
	        this.noiseOsc.connect(this.voiceGains[midiNoteKey]);
	        this.voices[midiNoteKey] = this.noiseOsc;
	    }
	}
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.default = NoiseOscillator;


/***/ },
/* 33 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";
	const FMOscillator_1 = __webpack_require__(8);
	const Constants_1 = __webpack_require__(1);
	const pulseCurve = new Float32Array(256);
	for (let i = 0; i < 128; i++) {
	    pulseCurve[i] = -1;
	    pulseCurve[i + 128] = 1;
	}
	const constantOneCurve = new Float32Array(2);
	constantOneCurve[0] = 1;
	constantOneCurve[1] = 1;
	class PulseOscillator extends FMOscillator_1.default {
	    constructor(context) {
	        super(context, 'pulse');
	        this.pulseWidth = 0;
	        this.widthGains = [];
	        for (let i = 0; i < 128; i++) {
	            this.widthGains[i] = this.context.createGain();
	        }
	    }
	    _noteOn(midiNote) {
	        const midiNoteKey = midiNote.toString();
	        const sawNode = this.context.createOscillator();
	        sawNode.type = Constants_1.default.WAVEFORM_TYPE.SAWTOOTH;
	        this.pulseShaper = this.context.createWaveShaper();
	        this.pulseShaper.curve = pulseCurve;
	        sawNode.connect(this.pulseShaper);
	        const widthGain = this.widthGains[midiNote];
	        widthGain.gain.value = this.pulseWidth;
	        widthGain.connect(this.pulseShaper);
	        const constantOneShaper = this.context.createWaveShaper();
	        constantOneShaper.curve = constantOneCurve;
	        sawNode.connect(constantOneShaper);
	        constantOneShaper.connect(widthGain);
	        sawNode.frequency.value = this.frequencyFromNoteNumber(midiNote);
	        sawNode.detune.value = this.detune + this.semitone;
	        this.pulseShaper.connect(this.voiceGains[midiNote]);
	        this.voices[midiNoteKey] = sawNode;
	    }
	    _onended() {
	        this.pulseShaper.disconnect();
	    }
	    setPulseWidth(width) {
	        this.pulseWidth = width;
	        this.widthGains.forEach(widthGain => {
	            widthGain.gain.value = width;
	        });
	    }
	}
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.default = PulseOscillator;


/***/ },
/* 34 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";
	const MIDI_1 = __webpack_require__(2);
	const Constants_1 = __webpack_require__(1);
	const FMOscillator_1 = __webpack_require__(8);
	const PulseOscillator_1 = __webpack_require__(33);
	const NoiseOscillator_1 = __webpack_require__(32);
	// TODO: move set state to Oscillator.js
	class Oscillators {
	    constructor(context, state) {
	        /***************************/
	        /*     private methods     */
	        /***************************/
	        this._setState = (state) => {
	            this._setMix(state.mix);
	            this.setFmAmount(state.osc1.fmGain);
	            this._setPulseWidth(state.pw);
	            this.setOscillator2Semitone(state.osc2.semitone);
	            this.setOscillator2Detune(state.osc2.detune);
	            this.toggleOsc2KbdTrack(state.osc2.kbdTrack);
	        };
	        this._setMix = (mix) => {
	            /* calculate and set volume mix between oscillators */
	            this.state.mix = mix;
	            const osc1GainValue = (1 - this.state.mix) / 2;
	            this.oscillator1Gain.gain.value = osc1GainValue;
	            const osc2GainValue = .5 - osc1GainValue;
	            this.oscillator2Gain.gain.value = osc2GainValue;
	        };
	        this._setPulseWidth = (pw) => {
	            this.state.pw = pw;
	            this.oscillator2.setPulseWidth(pw);
	        };
	        this._newOscillator = (waveformType) => {
	            if (waveformType == Constants_1.default.WAVEFORM_TYPE.PULSE) {
	                return new PulseOscillator_1.default(this.context);
	            }
	            else if (waveformType == Constants_1.default.WAVEFORM_TYPE.NOISE) {
	                return new NoiseOscillator_1.default(this.context);
	            }
	            else {
	                return new FMOscillator_1.default(this.context, waveformType);
	            }
	        };
	        this._setOscillator2Waveform = (waveform) => {
	            if (this.oscillator2.type !== Constants_1.default.WAVEFORM_TYPE.NOISE
	                && waveform !== Constants_1.default.WAVEFORM_TYPE.NOISE) {
	                if (waveform === Constants_1.default.WAVEFORM_TYPE.PULSE) {
	                    this._swapOsc2(new PulseOscillator_1.default(this.context), this.oscillator2Gain);
	                }
	                else {
	                    this.oscillator2.setWaveform(waveform);
	                }
	            }
	            else if (this.oscillator2.type !== Constants_1.default.WAVEFORM_TYPE.NOISE
	                && waveform === Constants_1.default.WAVEFORM_TYPE.NOISE) {
	                this._swapOsc2(new NoiseOscillator_1.default(this.context), this.oscillator2Gain);
	            }
	            else if (this.oscillator2.type === Constants_1.default.WAVEFORM_TYPE.NOISE
	                && waveform !== Constants_1.default.WAVEFORM_TYPE.NOISE) {
	                this._swapOsc2(new FMOscillator_1.default(this.context, waveform), this.oscillator2Gain);
	            }
	            this.state.osc2.waveformType = waveform;
	        };
	        this._swapOsc2 = (osc, gainB) => {
	            const now = this.context.currentTime;
	            for (const midiNote in this.oscillator2.voices) {
	                if (this.oscillator2.voices.hasOwnProperty(midiNote)) {
	                    this.oscillator2.noteOff(now, midiNote);
	                    osc.noteOn(midiNote);
	                }
	            }
	            this.oscillator2.voiceGains.forEach((oscGain, i) => 
	            // oscGain.disconnect(this.fmGains[i])
	            oscGain.disconnect());
	            this.oscillator2.disconnect(gainB);
	            this.oscillator2 = osc;
	            this.oscillator2.setPulseWidth(this.state.pw);
	            this.oscillator2.setDetune(this.state.osc2.detune);
	            this.oscillator2.setKbdTrack(this.state.osc2.kbdTrack);
	            this.oscillator2.setSemitone(this.state.osc2.semitone);
	            this.oscillator2.voiceGains.forEach((oscGain, i) => oscGain.connect(this.fmGains[i]));
	            this.oscillator2.connect(gainB);
	        };
	        this.setState = (state) => {
	            this._setState(state);
	            this.oscillator1.setWaveform(state.osc1.waveformType);
	            this._setOscillator2Waveform(state.osc2.waveformType);
	        };
	        this.setMix = (mix) => {
	            this._setMix(MIDI_1.default.normalizeValue(mix));
	        };
	        this.setOscillator2Semitone = (oscillatorSemitone) => {
	            this.state.osc2.semitone = oscillatorSemitone;
	            this.oscillator2.setSemitone(oscillatorSemitone);
	        };
	        this.setOscillator2Detune = (oscillatorDetune) => {
	            this.state.osc2.detune = oscillatorDetune;
	            this.oscillator2.setDetune(oscillatorDetune);
	        };
	        this.setFmAmount = (fmAmount) => {
	            const amount = 10 * fmAmount;
	            this.state.osc1.fmGain = amount;
	            for (let i = 0; i < Constants_1.default.MAX_NOTES; i++) {
	                this.fmGains[i].gain.value = amount;
	            }
	        };
	        this.setPulseWidth = (midiValue) => {
	            this._setPulseWidth(MIDI_1.default.logScaleToMax(midiValue, .9));
	        };
	        this.setOscillator1Waveform = (waveform) => {
	            const wf = waveform.toLowerCase();
	            if (Constants_1.default.OSC1_WAVEFORM_TYPES.indexOf(wf) !== -1) {
	                this.oscillator1.setWaveform(wf);
	            }
	            else {
	                throw new Error(`Invalid Waveform Type ${wf}`);
	            }
	        };
	        this.toggleOsc2KbdTrack = (enabled) => {
	            this.state.osc2.kbdTrack = enabled;
	            this.oscillator2.setKbdTrack(enabled);
	        };
	        this.setOscillator2Waveform = (waveform) => {
	            const wf = waveform.toLowerCase();
	            if (Constants_1.default.OSC2_WAVEFORM_TYPES.indexOf(wf)) {
	                this._setOscillator2Waveform(wf);
	            }
	            else {
	                throw new Error(`Invalid Waveform Type ${wf}`);
	            }
	        };
	        this.connect = (node) => {
	            this.oscillator2Gain.connect(node);
	            this.oscillator1Gain.connect(node);
	        };
	        this.disconnect = (node) => {
	            this.oscillator2Gain.disconnect(node);
	            this.oscillator1Gain.disconnect(node);
	        };
	        this.panic = () => {
	            this.oscillator1.panic();
	            this.oscillator2.panic();
	        };
	        this.noteOn = (midiNote, noteOnCb /*, velocity*/) => {
	            this.oscillator1.noteOn(midiNote, noteOnCb);
	            this.oscillator2.noteOn(midiNote, noteOnCb);
	        };
	        this.noteOff = (midiNote, noteOffCb /*, velocity*/) => {
	            this.oscillator1.noteOff(midiNote, noteOffCb);
	            this.oscillator2.noteOff(midiNote, noteOffCb);
	        };
	        this.context = context;
	        this.state = state;
	        /***************************/
	        /* AudioNode graph routing */
	        /***************************/
	        /* create oscillators gains */
	        this.oscillator1Gain = this.context.createGain();
	        this.oscillator2Gain = this.context.createGain();
	        /* create oscillator nodes */
	        this.oscillator1 =
	            new FMOscillator_1.default(this.context, state.osc1.waveformType);
	        this.oscillator2 = this._newOscillator(state.osc2.waveformType);
	        /* connect oscs with the previously mixed gains */
	        this.oscillator1.connect(this.oscillator1Gain);
	        this.oscillator2.connect(this.oscillator2Gain);
	        /* create Frequency Modulation gains */
	        this.fmGains = [];
	        for (let i = 0; i < 128; i++) {
	            this.fmGains[i] = this.context.createGain();
	            this.fmGains[i].gain.value = state.osc1.fmGain;
	            this.oscillator2.voiceGains[i].connect(this.fmGains[i]);
	            this.fmGains[i].connect(this.oscillator1.frequencyGains[i]);
	        }
	        this._setState(state);
	    }
	}
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.default = Oscillators;


/***/ },
/* 35 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";
	const Constants_1 = __webpack_require__(1);
	/* code copied from https://github.com/web-audio-components/overdrive */
	class Overdrive {
	    constructor(context, enabled) {
	        this.connect = (node) => {
	            this.output.connect(node.input ? node.input : node);
	        };
	        this.disconnect = () => {
	            this.output.disconnect();
	        };
	        this.setState = (enabled) => {
	            this.enabled = enabled;
	            this.input.disconnect();
	            if (enabled) {
	                this.input.connect(this._bandpass);
	            }
	            else {
	                this.input.connect(this.output);
	            }
	        };
	        this.context = context;
	        const params = Constants_1.default.OVERDRIVE_PARAMS;
	        /* internal AudioNodes */
	        this._bandpass = this.context.createBiquadFilter();
	        this._bpWet = this.context.createGain();
	        this._bpDry = this.context.createGain();
	        this._ws = this.context.createWaveShaper();
	        this._lowpass = this.context.createBiquadFilter();
	        /* AudioNode graph routing */
	        this.input = this.context.createGain();
	        this.output = this.context.createGain();
	        this.input.connect(this._bandpass);
	        this._bandpass.connect(this._bpWet);
	        this._bandpass.connect(this._bpDry);
	        this._bpWet.connect(this._ws);
	        this._bpDry.connect(this._ws);
	        this._ws.connect(this._lowpass);
	        this._lowpass.connect(this.output);
	        /* assign overdrive default values */
	        this._bandpass.frequency.value = params.color;
	        this._bpWet.gain.value = params.preBand;
	        this._lowpass.frequency.value = params.postCut;
	        this.drive = params.drive;
	        this._bpDry.gain.value = 1 - params.preBand;
	        this._bpWet.gain.value = params.preBand;
	        this._lowpass.frequency.value = params.postCut;
	        this.drive = params.drive;
	        // Inverted preBand value
	        this._bpDry.gain.value = params.preBand;
	        this.setState(enabled);
	    }
	    get preBand() {
	        return this._bpWet.gain.value;
	    }
	    set preBand(value) {
	        this._bpWet.gain.setValueAtTime(value, 0);
	        this._bpDry.gain.setValueAtTime(1 - value, 0);
	    }
	    get color() {
	        return this._bandpass.frequency.value;
	    }
	    set color(value) {
	        this._bandpass.frequency.setValueAtTime(value, 0);
	    }
	    get drive() {
	        return this._drive;
	    }
	    set drive(value) {
	        const k = value * 100, n = 22050, curve = new Float32Array(n), deg = Math.PI / 180;
	        this._drive = value;
	        for (let i = 0; i < n; i++) {
	            const x = i * 2 / n - 1;
	            curve[i] = (3 + k) * x * 20 * deg / (Math.PI + k * Math.abs(x));
	        }
	        this._ws.curve = curve;
	    }
	    get postCut() {
	        return this._lowpass.frequency.value;
	    }
	    set postCut(value) {
	        this._lowpass.frequency.setValueAtTime(value, 0);
	    }
	}
	exports.Overdrive = Overdrive;


/***/ },
/* 36 */
/***/ function(module, exports, __webpack_require__) {

	"use strict";
	const Oscillators_1 = __webpack_require__(34);
	const Filter_1 = __webpack_require__(31);
	const Amplifier_1 = __webpack_require__(30);
	const Overdrive_1 = __webpack_require__(35);
	const Constants_1 = __webpack_require__(1);
	class Synth {
	    constructor(state) {
	        this.setState = (state) => {
	            this.amplifier.setState(state.amp);
	            this.overdrive.setState(state.overdrive);
	            this.filter.setState(state.filter);
	            this.oscillators.setState(state.oscs);
	        };
	        this.onMIDIMessage = (data) => {
	            //console.log(data)
	            // var cmd = data[0] >> 4
	            // var channel = data[0] & 0xf
	            // channel agnostic message type
	            const type = data[0] & 0xf0;
	            const note = data[1];
	            //const velocity = data[2]
	            switch (type) {
	                case Constants_1.default.MIDI_EVENT.NOTE_ON:
	                    this.oscillators.noteOn(note, this.amplifier.adsr.on(0, 1));
	                    this.filter.noteOn();
	                    break;
	                case Constants_1.default.MIDI_EVENT.NOTE_OFF:
	                    this.oscillators.noteOff(note, this.amplifier.adsr.off);
	                    this.filter.noteOff();
	                    break;
	            }
	        };
	        this.context = new AudioContext;
	        this.amplifier = new Amplifier_1.Amplifier(this.context, state.amp);
	        this.overdrive = new Overdrive_1.Overdrive(this.context, state.overdrive);
	        this.overdrive.connect(this.amplifier.output);
	        this.filter = new Filter_1.Filter(this.context, state.filter);
	        this.filter.connect(this.overdrive.input);
	        this.oscillators = new Oscillators_1.default(this.context, state.oscs);
	        this.oscillators.connect(this.filter.input);
	    }
	}
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.default = Synth;


/***/ },
/* 37 */
/***/ function(module, exports) {

	"use strict";
	class PresetManager {
	    constructor(presets) {
	        this.previous = () => {
	            // cycle through bank
	            this.currentPresetIndex -= 1;
	            if (this.currentPresetIndex == -1) {
	                this.currentPresetIndex = this.presets.length - 1;
	            }
	            return this.current();
	        };
	        this.presets = presets;
	        this.currentPresetIndex = -1;
	    }
	    current() {
	        return this.presets[this.currentPresetIndex];
	    }
	    next() {
	        this.currentPresetIndex += 1;
	        // cycle through bank
	        if (this.currentPresetIndex == this.presets.length) {
	            this.currentPresetIndex = 0;
	        }
	        return this.current();
	    }
	}
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.default = PresetManager;


/***/ },
/* 38 */
/***/ function(module, exports) {

	module.exports = function() { throw new Error("define cannot be used indirect"); };


/***/ },
/* 39 */
/***/ function(module, exports) {

	module.exports = function(module) {
		if(!module.webpackPolyfill) {
			module.deprecate = function() {};
			module.paths = [];
			// module.parent = undefined by default
			module.children = [];
			module.webpackPolyfill = 1;
		}
		return module;
	}


/***/ }
/******/ ]);