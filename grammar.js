// let _ = require('../node_modules/underscore/underscore.js')

// map from type of words to actual words
const GRAMMAR_VAR = {
  "SUBJ": ["the green block", "the blue block"],
  "V": ["falls", "fall"],
  'CONJ': ["but", "if", "and"],
  'NEG': ["neither", "nor"],
  'NOT': ["does not"],
  'ADV1': ["as well"],
  'ADV2': ["probably"]
}

const CONSTITUENTS = _.values(GRAMMAR_VAR);

// which words are clickable after which type of words
const GRAMMAR_RULE = {
  'S': ["SUBJ", "neither", "if", "ADV2"],
  'SUBJ': ['NEG', 'V', 'CONJ', "ADV2", 'NOT'],
  'V': ['SUBJ', 'CONJ', 'NEG', 'ADV1'],
  'CONJ': ['SUBJ'],
  'NEG': ['SUBJ'],
  'NOT': ['fall'],
  'ADV1': [],
  'ADV2': ['SUBJ', 'V', 'NOT']
}
// each word must appear in this array to get a color
let WORD_GROUPS = [
  {words: GRAMMAR_VAR.SUBJ,
   col: 'black'
 },
  {
    words: GRAMMAR_VAR.V,
    col: 'orange'
  },
  {
    words: GRAMMAR_VAR.CONJ,
    col: 'blue'
  },
  {
    words: GRAMMAR_VAR.NEG.concat(GRAMMAR_VAR.NOT),
    col: 'red'
  },
  {
    words: GRAMMAR_VAR.ADV1,
    col: 'light-blue'
  },
  {
    words: GRAMMAR_VAR.ADV2,
    col: 'purple'
  }
];
let WORDS = _.flatten(_.map(_.values(WORD_GROUPS), 'words'));
// console.log(WORDS)

let shownNext = function (last, sentence='') {
  let arr = Object.keys(GRAMMAR_RULE)
    .includes(last) ? GRAMMAR_RULE[last] :
    Object.keys(GRAMMAR_VAR)
    .includes(last) ? GRAMMAR_VAR[last] :
    GRAMMAR_RULE[_.filter(Object.keys(GRAMMAR_VAR), function (key) {
      return GRAMMAR_VAR[key].includes(last);
    })[0]];
  let symbols = _.reduce(arr, function (acc, val) {
    return acc.concat(val == val.toLowerCase() ? val : GRAMMAR_VAR[val]);
  }, []);
  // special rules

  // nor only possible if neither had been selected +
  // neither/nor only selectable once!
  symbols = !sentence.includes('neither') ? _.without(symbols, 'nor') :
    _.without(symbols, 'neither');
  symbols = sentence.includes('nor') ? _.without(symbols, 'nor') : symbols;

  // neither only at beginning
  symbols = sentence != '' ? _.without(symbols, 'neither') : symbols;

  // probably not after conjunctions
  symbols = _.every(GRAMMAR_VAR.CONJ.concat(GRAMMAR_VAR.NEG), function(conj) {
    return !sentence.includes(conj)
  }) ? symbols : _.without(symbols, 'probably')

  // conjunctions not after 'probably'
  symbols = sentence.includes('probably') ? _.without(symbols, 'and', 'but', 'if') : symbols;

  // can the currently built sentence be extended further to an utterance?
  let poss_utts = _.filter(UTTERANCES, function(utt){
    return utt.startsWith(sentence.trim()) && sentence !== utt;
  });
  symbols = poss_utts.length == 0 ? [] : symbols;
  return symbols
}



let templates = [
  "the COL1 block [probably] falls",
  "the COL1 block [probably] does not fall",
  "probably the COL1 block falls",
  "probably the COL1 block does not fall",

  "the COL1 block and the COL2 block fall",
  "the COL1 block falls and the COL2 block falls [as well]",
  "the COL1 block falls but the COL2 block does not fall",
  "the COL1 block falls and the COL2 block does not fall",
  "neither the COL1 block nor the COL2 block fall",
  "neither the COL1 block nor the COL2 block falls",
  "neither the COL1 block falls nor the COL2 block falls",

  "if the COL1 block falls the COL2 block falls [as well]",
  "if the COL1 block does not fall the COL2 block falls",
  "if the COL1 block does not fall the COL2 block does not fall",
  "if the COL1 block falls the COL2 block does not fall"
]

let UTTERANCES = [];
_.map(templates, function(u){
    let gb = u.replace("COL1", "green").replace("COL2", "blue");
    let bg = u.replace("COL1", "blue").replace("COL2", "green");

    let u1 = gb.replace("[", "").replace("]", "").trim();
    let u2 = gb.replace(/\[[^\]]*\] */g, "").trim() // replace what is inside []-brackets
    let u3 = bg.replace("[", "").replace("]", "").trim();
    let u4 = bg.replace(/\[[^\]]*\] */g, "").trim() // replace what is inside []-brackets

    UTTERANCES.push.apply(UTTERANCES, [u1, u2, u3, u4]);
});

UTTERANCES = Array.from(new Set(UTTERANCES));

// let symbols = shownNext('S')
// let i = _.random(0, symbols.length - 1)
// let selected = typeof(symbols[i]) == 'string' ? symbols[i] : _.sample(symbols[i]);