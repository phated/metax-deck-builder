var fs = require('fs');
var { GraphQLClient } = require('graphql-request');

var data = require('./data/metax-db.json');

var allCards = fs.readFileSync('./queries/AllCards.graphql', 'utf8');
var queries = {
  Character: fs.readFileSync('./queries/CreateCharacterCard.graphql', 'utf8'),
  Event: fs.readFileSync('./queries/CreateEventCard.graphql', 'utf8'),
  Battle: fs.readFileSync('./queries/CreateBattleCard.graphql', 'utf8'),
};
var queriesWithPreview = {
  Character: fs.readFileSync('./queries/CreateCharacterCardWithPreview.graphql', 'utf8'),
  Event: fs.readFileSync('./queries/CreateEventCardWithPreview.graphql', 'utf8'),
  Battle: fs.readFileSync('./queries/CreateBattleCardWithPreview.graphql', 'utf8'),
};

var client = new GraphQLClient('https://api.graph.cool/simple/v1/cjerpcdas51ih01414psrg6wa', {
  headers: {
    Authorization: `Bearer ${GRAPHCOOL_TOKEN}`,
  },
})

async function run() {
  var cards = await client.request(allCards)
    .then(function(data) {
      return data.allCards;
    });

  var allUIDs = cards.map(function(card) {
    return card.uid;
  });

  var filteredData = data.filter(function(row) {
    return !allUIDs.includes(row.uid);
  });

  var requests = filteredData.map(function(row) {
    var query;
    if (row.previewer) {
      query = queriesWithPreview[row.type];
    } else {
      query = queries[row.type];
    }
    if (!query) {
      throw new Error('Query not found');
    }
    return client.request(query, row);
  });

  return Promise.all(requests);
}

run().then(console.log).catch(console.error);
