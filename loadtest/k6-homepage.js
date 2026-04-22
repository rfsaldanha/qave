import http from 'k6/http';
import { check, sleep } from 'k6';

const baseUrl = (__ENV.BASE_URL || 'https://qave-app-4plrr.ondigitalocean.app').replace(/\/+$/, '');
const localAssetPattern = /(src|href)="(\/[^"]+\.(?:js|css))"/g;

function unique(items) {
  return [...new Set(items)];
}

function extractLocalAssets(html) {
  const assets = [];
  let match;

  while ((match = localAssetPattern.exec(html)) !== null) {
    assets.push(match[2]);
  }

  return unique(assets).slice(0, 6);
}

export const options = {
  thresholds: {
    http_req_failed: ['rate<0.02'],
    http_req_duration: ['p(95)<2000'],
    checks: ['rate>0.99'],
  },
  scenarios: {
    homepage_ramp: {
      executor: 'ramping-vus',
      startVUs: 0,
      gracefulRampDown: '10s',
      stages: [
        { duration: __ENV.STAGE_1_DURATION || '30s', target: Number(__ENV.STAGE_1_VUS || 10) },
        { duration: __ENV.STAGE_2_DURATION || '1m', target: Number(__ENV.STAGE_2_VUS || 25) },
        { duration: __ENV.STAGE_3_DURATION || '30s', target: 0 },
      ],
    },
  },
};

export default function () {
  const homepage = http.get(`${baseUrl}/`, {
    tags: { name: 'homepage' },
  });

  const homepageOk = check(homepage, {
    'homepage returned 200': (res) => res.status === 200,
    'homepage contains title': (res) => res.body.includes('Que ave é essa?'),
    'homepage contains shiny bootstrap': (res) => res.body.includes('shiny.min.js'),
  });

  if (!homepageOk) {
    sleep(1);
    return;
  }

  const assetPaths = extractLocalAssets(homepage.body);

  if (assetPaths.length > 0) {
    const assetResponses = http.batch(
      assetPaths.map((path) => [
        'GET',
        `${baseUrl}${path}`,
        null,
        { tags: { name: 'static_asset', asset_path: path } },
      ])
    );

    check(assetResponses, {
      'all sampled assets returned 200': (responses) =>
        responses.every((response) => response.status === 200),
    });
  }

  sleep(Number(__ENV.SLEEP_SECONDS || 1));
}
