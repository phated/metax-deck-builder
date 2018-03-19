var fs = require('fs');
var { GraphQLClient } = require('graphql-request');

var allData = fs.readFileSync(`${__dirname}/queries/AllData.graphql`, 'utf8');


var client = new GraphQLClient('https://api.graph.cool/simple/v1/metaxdb')

async function run() {
  client.rawRequest
  var cards = await client.request(allData)
    .then(function(data) {
      return data.allCards;
    });

  // console.log(cards);
  fs.writeFileSync(`${process.cwd()}/data/metax.v2.json`, JSON.stringify(cards, null, 2));
}

run().then(console.log).catch(console.error);
