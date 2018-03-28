var csv = require('csvtojson');

function stringOrUndefined(item) {
  if (item) {
    return item;
  }
}

var IDs = require('../graphcool-ids');
function idOrUndefined(item, heading) {
  var idList = IDs[heading];
  if (!idList) {
    return
  }

  return idList[item];
}

function toStats(item, heading, resultRow) {
  var id = idOrUndefined(item, heading);
  if (id) {
    resultRow.statsIds = resultRow.statsIds || [];
    resultRow.statsIds.push(id);
  }
}

function toTrait(item, heading, resultRow) {
  var id = idOrUndefined(item, heading);
  if (id) {
    resultRow.traitId = id;
  }
}

var conv = csv({
  toArrayString: true,
  colParser: {
    uid: 'string',
    rarity: 'string',
    number: 'number',
    set: 'string',
    title: 'string',
    subtitle: stringOrUndefined,
    type: 'string',
    trait: toTrait,
    mp: 'number',
    symbol: stringOrUndefined,
    effect: stringOrUndefined,
    strength: toStats,
    intelligence: toStats,
    special: toStats,
    imageUrl: 'string',
    previewUrl: stringOrUndefined,
    previewer: stringOrUndefined
  }
});

conv.fromFile(__dirname + '/metax-db.csv').pipe(process.stdout);
