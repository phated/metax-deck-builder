var fs = require('fs');
var { GraphQLClient } = require('graphql-request');

var data = require('./data/metax-db.json');

var allCards = fs.readFileSync('./queries/AllCards.graphql', 'utf8');
var setCardEffect = fs.readFileSync('./queries/SetCardEffect.graphql', 'utf8');

var client = new GraphQLClient('https://api.graph.cool/simple/v1/metaxdb', {
  headers: {
    Authorization: `Bearer ${process.env.GRAPHCOOL_TOKEN}`,
  },
})

async function run() {
  var cards = await client.request(allCards)
    .then(function (data) {
      return data.allCards;
    });

  var effectsWithIds = cards.map(function (card) {
    var { symbol, effect } = data.find(function (row) {
      return row.uid === card.uid;
    });

    return { id: card.id, symbol, effect }
  });

  // console.log(effectsWithIds)
  var requests = effectsWithIds.map(function (row) {
    return client.request(setCardEffect, row);
  });

  return Promise.all(requests);
}

run().then(console.log).catch(console.error);
