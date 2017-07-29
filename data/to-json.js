#!/usr/bin/env node

var csv = require('csvtojson');

function numberOrNull(item) {
  if (parseInt(item, 10) == item) {
    return parseInt(item, 10);
  } else {
    return null;
  }
}

var conv = csv({
  toArrayString: true,
  colParser: {
    id: 'string',
    title: 'string',
    card_type: 'string',
    trait: 'string',
    mp: 'number',
    effect: 'string',
    strength: numberOrNull,
    intelligence: numberOrNull,
    special: numberOrNull,
    field10: 'omit',
    image_url: function(item) {
      return item.replace('http://metaxdb.com', '');
    }
  }
});

conv.fromFile(__dirname + '/metax-db.csv').pipe(process.stdout);