function request(cb) {
  var url = '/mxdeckregform.pdf';

  var xhr = new XMLHttpRequest();
  xhr.open('GET', url, true);
  xhr.responseType = 'arraybuffer';

  xhr.onload = function () {
    if (this.status == 200) {
      cb(null, this.response);
    } else {
      cb(new Error('failed to load URL (code: ' + this.status + ')'));
    }
  };

  xhr.send();
}

var app = Elm.Main.fullscreen();

app.ports.storeSession.subscribe(function (session) {
  localStorage.session = session;
});

app.ports.loadSession.subscribe(function (deckId) {
  app.ports.onSessionLoaded.send(localStorage.session)
})

var characterKeys = [
  { quantity: ' IN DECKRow1', title: 'Character Title - Subtitle_1' },
  { quantity: 'IN DECKRow2', title: 'Character Title - Subtitle_2' },
  { quantity: 'IN DECKRow3', title: 'Character Title - Subtitle_3' },
  { quantity: 'IN DECKRow4', title: 'Character Title - Subtitle_4' },
  { quantity: 'IN DECKRow5', title: 'Character Title - Subtitle_5' },
  { quantity: 'IN DECKRow6', title: 'Character Title - Subtitle_6' },
  { quantity: 'IN DECKRow7', title: 'Character Title - Subtitle_7' },
  { quantity: 'IN DECKRow8', title: 'Character Title - Subtitle_8' },
  { quantity: 'IN DECKRow9', title: 'Character Title - Subtitle_9' },
  { quantity: 'IN DECKRow10', title: 'Character Title - Subtitle_10' },
];

var eventKeys = [
  { quantity: 'IN DECKRow1', title: 'Event TitleRow1' },
  { quantity: ' IN DECKRow2', title: 'Event TitleRow2' },
  { quantity: ' IN DECKRow3', title: 'Event TitleRow3' },
  { quantity: ' IN DECKRow4', title: 'Event TitleRow4' },
  { quantity: ' IN DECKRow5', title: 'Event TitleRow5' },
  { quantity: ' IN DECKRow1_2', title: 'Event TitleRow1_2' },
  { quantity: ' IN DECKRow2_2', title: 'Event TitleRow2_2' },
  { quantity: ' IN DECKRow3_2', title: 'Event TitleRow3_2' },
  { quantity: ' IN DECKRow4_2', title: 'Event TitleRow4_2' },
  { quantity: ' IN DECKRow5_2', title: 'Event TitleRow5_2' },
];

var battleKeys = [
  { quantity: ' IN DECKRow1_3', title: 'Battle Card TitleRow1', id: 'SetHRow1' },
  { quantity: ' IN DECKRow2_3', title: 'Battle Card TitleRow2', id: 'SetHRow2' },
  { quantity: ' IN DECKRow3_3', title: 'Battle Card TitleRow3', id: 'SetHRow3' },
  { quantity: ' IN DECKRow4_3', title: 'Battle Card TitleRow4', id: 'SetHRow4' },
  { quantity: ' IN DECKRow5_3', title: 'Battle Card TitleRow5', id: 'SetHRow5' },
  { quantity: ' IN DECKRow6', title: 'Battle Card TitleRow6', id: 'SetHRow6' },
  { quantity: ' IN DECKRow7', title: 'Battle Card TitleRow7', id: 'SetHRow7' },
  { quantity: ' IN DECKRow8', title: 'Battle Card TitleRow8', id: 'SetHRow8' },
  { quantity: ' IN DECKRow9', title: 'Battle Card TitleRow9', id: 'SetHRow9' },
  { quantity: ' IN DECKRow10', title: 'Battle Card TitleRow10', id: 'SetHRow10' },
  { quantity: ' IN DECKRow11', title: 'Battle Card TitleRow11', id: 'SetHRow11' },
  { quantity: ' IN DECKRow12', title: 'Battle Card TitleRow12', id: 'SetHRow12' },
  { quantity: ' IN DECKRow13', title: 'Battle Card TitleRow13', id: 'SetHRow13' },
];

app.ports.exportSession.subscribe(function (session) {
  var decklist = JSON.parse(session);

  request(fillForm);

  function fillForm(err, pdf) {
    if (err) {
      throw err;
    }

    var form = pdfform();
    var rows = {};

    // TODO: This should probably be done in Elm!
    var characters = decklist.filter(function (card) {
      return card.card_type === 'Character';
    });
    rows = characters.reduce(function (result, card, idx) {
      var keys = characterKeys[idx];
      result[keys.quantity] = [card.quantity];
      result[keys.title] = [card.title];
      return result;
    }, rows);

    var battle = decklist.filter(function (card) {
      return card.card_type === 'Battle';
    });
    rows = battle.reduce(function (result, card, idx) {
      var keys = battleKeys[idx];
      result[keys.quantity] = [card.quantity];
      result[keys.title] = [card.title];
      result[keys.id] = [card.id];
      return result;
    }, rows);

    var events = decklist.filter(function (card) {
      return card.card_type === 'Event';
    });
    rows = events.reduce(function (result, card, idx) {
      var keys = eventKeys[idx];
      result[keys.quantity] = [card.quantity];
      result[keys.title] = [card.title];
      return result;
    }, rows);

    var result = form.transform(pdf, rows);
    var blob = new Blob([result], { type: 'application/pdf' });
    saveAs(blob, 'metax.decklist.pdf');
  }
});

window.addEventListener("storage", function (event) {
  if (event.storageArea === localStorage && event.key === "session") {
    app.ports.onSessionChange.send(event.newValue);
  }
}, false);
