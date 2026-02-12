import { chromium } from 'playwright';

const APP_URL = 'http://localhost:8081';
const BACKEND_URL = 'http://100.108.213.83:8080';

async function testWebSocket() {
  console.log('Launching browser...');
  const browser = await chromium.launch({ headless: false }); // headless: false to see what's happening
  const context = await browser.newContext();
  const page = await context.newPage();

  // Listen to console logs from the page
  page.on('console', msg => {
    console.log(`[PAGE ${msg.type()}]: ${msg.text()}`);
  });

  // Listen to WebSocket events
  page.on('websocket', ws => {
    console.log(`[WS] WebSocket opened: ${ws.url()}`);
    
    ws.on('framereceived', frame => {
      console.log(`[WS] Frame received: ${frame.payload?.toString().slice(0, 100)}...`);
    });
    
    ws.on('framesent', frame => {
      console.log(`[WS] Frame sent: ${frame.payload?.toString().slice(0, 100)}...`);
    });
    
    ws.on('close', () => {
      console.log('[WS] WebSocket closed');
    });
  });

  console.log(`Navigating to ${APP_URL}...`);
  await page.goto(APP_URL);

  // Wait for the app to load
  await page.waitForTimeout(3000);

  // Check if there's a settings screen (first time setup)
  const urlInput = await page.$('input[placeholder*="URL"]');
  if (urlInput) {
    console.log('Found settings screen, entering backend URL...');
    await urlInput.fill(BACKEND_URL);
    
    // Look for connect/save button
    const connectButton = await page.$('text=Connect') || await page.$('text=Save');
    if (connectButton) {
      await connectButton.click();
      await page.waitForTimeout(2000);
    }
  }

  console.log('Waiting for 30 seconds to observe WebSocket behavior...');
  await page.waitForTimeout(30000);

  // Take a screenshot
  await page.screenshot({ path: '/tmp/dendrite_test.png' });
  console.log('Screenshot saved to /tmp/dendrite_test.png');

  await browser.close();
  console.log('Test complete');
}

testWebSocket().catch(console.error);
