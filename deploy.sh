rm dist/*
npm run build
sed -i 's,http://localhost:5001,http://ns334712.ip-178-33-239.eu/api,g' dist/index.html
scp -r dist/* mmz@ns334712.ip-178-33-239.eu:speechtotext-api/dist
