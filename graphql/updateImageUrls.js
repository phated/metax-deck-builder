var fs = require('fs');
var { GraphQLClient } = require('graphql-request');

var GetImageUrls = fs.readFileSync('./queries/ImageUrls.graphql', 'utf8');
var UpdateImageUrl = fs.readFileSync('./queries/UpdateImageUrl.graphql', 'utf8');

var client = new GraphQLClient('https://api.graph.cool/simple/v1/metaxdb', {
  headers: {
    Authorization: `Bearer ${process.env.GRAPHCOOL_TOKEN}`,
  },
})

async function run() {
  var cards = await client.request(GetImageUrls)
    .then(function (data) {
      return data.allCards;
    });

  var updatedCards = cards.map(function (card) {
    card.imageUrl = card.imageUrl.replace('http://', 'https://');
    return card;
  });

  var requests = updatedCards.map(function (row) {
    return client.request(UpdateImageUrl, row);
  });

  return Promise.all(requests);
}

run().then(console.log).catch(console.error);
