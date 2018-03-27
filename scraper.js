var fs = require('fs');
var path = require('path');

var cheerio = require('cheerio');
var { default: howard, text, buffer } = require('howard');

var url = "https://paninigames.com/attack-on-titan-checklist/";

var outdir = path.join(__dirname, 'scraped-images');

async function load() {
  var page = await text(howard(url));
  // console.log(page);
  var $ = cheerio.load(page);

  var rows = $('#post-14041 > div > table > tbody > tr').not(':first-of-type');
  rows.each(async function (idx, el) {
    var cells = $(el).find('td');
    // console.log(cells.length)
    if (cells.length == 2) {
      var uid = $(cells[0]).text();
      var imageUrl = $(cells[1]).find('a').attr('href');
      // console.log(uid, imageUrl)
      if (uid && imageUrl) {
        var img = await buffer(howard(imageUrl));
        // console.log(img);
        var ext = path.extname(imageUrl);
        // console.log(ext);
        var outpath = path.join(outdir, uid + ext)
        fs.writeFileSync(outpath, img);
      }
    }
  });
}

load()
