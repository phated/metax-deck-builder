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
    set: 'string',
    title: 'string',
    card_type: 'string',
    trait: 'string',
    mp: 'number',
    effect: 'string',
    strength: numberOrNull,
    intelligence: numberOrNull,
    special: numberOrNull,
    image_url: function(item) {
      return item.replace('http://metaxdb.com', '');
    },
    preview_url: 'string',
    previewer: 'string'
  }
});

conv.fromFile(__dirname + '/metax-db.csv').pipe(process.stdout);
