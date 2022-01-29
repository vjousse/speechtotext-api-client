import { Elm } from '../src/Main.elm';

var flags = JSON.parse(localStorage.getItem('storage'));

if (flags === null) {
  flags = {};
}

flags['server_url'] = serverUrl;

const app = Elm.Main.init({
  flags: flags,
});

app.ports.save_.subscribe((storage) => {
  console.log('Saving to storage');
  localStorage.setItem('storage', JSON.stringify(storage));
  app.ports.load_.send(storage);
});
