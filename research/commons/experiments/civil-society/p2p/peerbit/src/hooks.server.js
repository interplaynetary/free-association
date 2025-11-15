/** @type {import('@sveltejs/kit').Handle} */
export function handle({ event, resolve }) {
  // Special handling for Chrome DevTools request
  if (event.url.pathname === '/.well-known/appspecific/com.chrome.devtools.json') {
    return new Response(JSON.stringify({}), {
      headers: {
        'content-type': 'application/json'
      }
    });
  }

  return resolve(event);
} 