(function(){const e=document.createElement("link").relList;if(e&&e.supports&&e.supports("modulepreload"))return;for(const r of document.querySelectorAll('link[rel="modulepreload"]'))o(r);new MutationObserver(r=>{for(const n of r)if(n.type==="childList")for(const s of n.addedNodes)s.tagName==="LINK"&&s.rel==="modulepreload"&&o(s)}).observe(document,{childList:!0,subtree:!0});function t(r){const n={};return r.integrity&&(n.integrity=r.integrity),r.referrerPolicy&&(n.referrerPolicy=r.referrerPolicy),r.crossOrigin==="use-credentials"?n.credentials="include":r.crossOrigin==="anonymous"?n.credentials="omit":n.credentials="same-origin",n}function o(r){if(r.ep)return;r.ep=!0;const n=t(r);fetch(r.href,n)}})();const ae=`#version 300 es
precision highp float;
out vec4 fragColor;

uniform vec3 iResolution;
uniform float iTime;
uniform float iFrame;
uniform float timeScale;
uniform float hueShift;
uniform float saturation;
uniform float lightness;

#define POINTS 32
#define PI 3.1415926536
#define TAU (2.0 * PI)
#define S(a,b,t) smoothstep(a,b,t)

mat2 rot(float a) {
    float s = sin(a);
    float c = cos(a);
    return mat2(c, -s, s, c);
}

// HSV to RGB conversion
vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

// RGB to HSV conversion
vec3 rgb2hsv(vec3 c) {
    vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
    vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

// Apply hue shift to RGB color
vec3 applyHueShift(vec3 color, float shift) {
    vec3 hsv = rgb2hsv(color);
    hsv.x = fract(hsv.x + shift); // Rotate hue by shift amount (0-1 range)
    return hsv2rgb(hsv);
}

// Apply saturation adjustment to RGB color
vec3 applySaturation(vec3 color, float satFactor) {
    vec3 hsv = rgb2hsv(color);
    hsv.y = clamp(hsv.y * satFactor, 0.0, 1.0); // Adjust saturation
    return hsv2rgb(hsv);
}

// Add dithering function
float dither(vec2 uv) {
    return fract(sin(dot(uv, vec2(12.9898, 78.233))) * 43758.5453);
}

// Apply lightness adjustment to RGB color
vec3 applyLightness(vec3 color, float lightFactor) {
    // Convert to grayscale for more dramatic effect
    float gray = dot(color, vec3(0.299, 0.587, 0.114));

    // Shift the curve to make 0 match previous 1
    float shiftedFactor = (lightFactor * 14.0 + 1.0) / 15.0;
    float curve = shiftedFactor * shiftedFactor * 0.9;

    // Mix between original color and white/black based on lightness
    vec3 result;
    if (lightFactor > 0.5) {
        // Mix with white for lighter values, but cap at 0.95
        float mixAmount = min((curve - 0.5) * 2.0, 0.95);
        result = mix(color, vec3(1.0), mixAmount);
    } else {
        // Mix with black for darker values, but cap at 0.95
        float mixAmount = min(curve * 2.0, 0.95);
        result = mix(vec3(0.1), color, mixAmount);
    }

    // Add dithering to break up color bands
    float ditherAmount = (1.0 - lightFactor) * 0.02; // More dither in darker areas
    vec2 uv = gl_FragCoord.xy / iResolution.xy;
    float noise = dither(uv) * ditherAmount;
    result += vec3(noise);

    return result;
}

vec2 hash(vec2 p) {
    p = vec2(dot(p, vec2(2127.1, 81.17)), dot(p, vec2(1269.5, 283.37)));
    return fract(sin(p)*43758.5453);
}

float noise(in vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);
    vec2 u = f*f*(3.0-2.0*f);
    float n = mix(mix(dot(-1.0+2.0*hash(i + vec2(0.0, 0.0)), f - vec2(0.0, 0.0)),
    dot(-1.0+2.0*hash(i + vec2(1.0, 0.0)), f - vec2(1.0, 0.0)), u.x),
    mix(dot(-1.0+2.0*hash(i + vec2(0.0, 1.0)), f - vec2(0.0, 1.0)),
    dot(-1.0+2.0*hash(i + vec2(1.0, 1.0)), f - vec2(1.0, 1.0)), u.x), u.y);
    return 0.5 + 0.5*n;
}
`,ce=`
vec4 shader(vec2 fragCoord) {
  vec2 uv = fragCoord / iResolution.xy;
  float aspectRatio = iResolution.x / iResolution.y;
  vec2 tuv = uv - .5;
  float t = iTime * timeScale;  // Use timeScale for dynamic speed
  float degree = noise(vec2(t * 0.1, tuv.x*tuv.y));  // Slow rotation
  tuv.y *= 1./aspectRatio;
  tuv *= rot(radians((degree-.5)*720.+180.));
  tuv.y *= aspectRatio;
  float frequency = 5.;
  float amplitude = 30.;
  float speed = t * 2.0;  // Use timeScale for speed
  tuv.x += sin(tuv.y*frequency+speed)/amplitude;
  tuv.y += sin(tuv.x*frequency*1.5+speed)/(amplitude*.5);
  vec3 amberYellow = vec3(299, 186, 137) / vec3(255);
  vec3 deepBlue = vec3(49, 98, 238) / vec3(255);
  vec3 pink = vec3(246, 146, 146) / vec3(255);
  vec3 blue = vec3(89, 181, 243) / vec3(255);
  vec3 purpleHaze = vec3(105, 49, 245) / vec3(255);
  vec3 swampyBlack = vec3(32, 42, 50) / vec3(255);
  vec3 persimmonOrange = vec3(233, 51, 52) / vec3(255);
  vec3 darkAmber = vec3(233, 160, 75) / vec3(255);
  float cycle = sin(t * 0.5);  // Slower color cycling
  float mixT = (sign(cycle) * pow(abs(cycle), 0.6) + 1.) / 2.;
  vec3 color1 = mix(amberYellow, purpleHaze, mixT);
  vec3 color2 = mix(deepBlue, swampyBlack, mixT);
  vec3 color3 = mix(pink, persimmonOrange, mixT);
  vec3 color4 = mix(blue, darkAmber, mixT);
  vec3 layer1 = mix(color3, color2, smoothstep(-.3, .2, (tuv*rot(radians(-5.))).x));
  vec3 layer2 = mix(color4, color1, smoothstep(-.3, .2, (tuv*rot(radians(-5.))).x));
  vec3 color = mix(layer1, layer2, smoothstep(.5, -.3, tuv.y));

  // Apply hue shift to the final color
  color = applyHueShift(color, hueShift);

  // Apply saturation adjustment
  color = applySaturation(color, saturation);

  // Apply lightness adjustment
  color = applyLightness(color, lightness);

  return vec4(color, 1.0);
}
`,le=`
vec3 hash3d(vec3 p) {
  p = vec3(dot(p, vec3(127.1, 311.7, 74.7)), dot(p, vec3(269.5, 183.3, 246.1)),
          dot(p, vec3(113.5, 271.9, 124.6)));
  p = -1.0 + 2.0 * fract(sin(p) * 43758.5453123);
  return p;
}

float noise3d(in vec3 p) {
  vec3 i = floor(p);
  vec3 f = fract(p);
  vec3 u = f * f * (3.0 - 2.0 * f);
  return mix(
      mix(mix(dot(hash3d(i + vec3(0.0, 0.0, 0.0)), f - vec3(0.0, 0.0, 0.0)),
              dot(hash3d(i + vec3(1.0, 0.0, 0.0)), f - vec3(1.0, 0.0, 0.0)),
              u.x),
          mix(dot(hash3d(i + vec3(0.0, 1.0, 0.0)), f - vec3(0.0, 1.0, 0.0)),
              dot(hash3d(i + vec3(1.0, 1.0, 0.0)), f - vec3(1.0, 1.0, 0.0)),
              u.x),
          u.y),
      mix(mix(dot(hash3d(i + vec3(0.0, 0.0, 1.0)), f - vec3(0.0, 0.0, 1.0)),
              dot(hash3d(i + vec3(1.0, 0.0, 1.0)), f - vec3(1.0, 0.0, 1.0)),
              u.x),
          mix(dot(hash3d(i + vec3(0.0, 1.0, 1.0)), f - vec3(0.0, 1.0, 1.0)),
              dot(hash3d(i + vec3(1.0, 1.0, 1.0)), f - vec3(1.0, 1.0, 1.0)),
              u.x),
          u.y),
      u.z);
}

vec4 shader(vec2 fragCoord) {
  const int layers = 5;
  const float baseSpeed = 0.25; // Base speed
  const float scale = 1.2;

  vec2 uv = (fragCoord - iResolution.xy - .5) / iResolution.y;
  float t = iTime * baseSpeed * timeScale; // Use timeScale for dynamic speed
  uv *= scale;
  float h =
      noise3d(vec3(uv * 2., t)); // Time as z-coordinate for continuous noise
  for (int n = 1; n < layers; n++) {
    float i = float(n);
    uv -= vec2(0.7 / i * sin(i * uv.y + i + t * 2.0 + h * i) +
                  0.8, // Reduced from 5.0 to 2.0
              0.4 / i * sin(uv.x + 4. - i + h + t * 2.0 + 0.3 * i) +
                  1.6); // Reduced from 5.0 to 2.0
  }
  uv -=
      vec2(1.2 * sin(uv.x + t + h) + 1.8, 0.4 * sin(uv.y + t + 0.3 * h) + 1.6);
  vec3 col = vec3(.5 * sin(uv.x) + 0.5, .5 * sin(uv.x + uv.y) + 0.5,
                  .5 * sin(uv.y) + 0.8) *
            0.8;

  // Apply hue shift to the final color
  col = applyHueShift(col, hueShift);

  // Apply saturation adjustment
  col = applySaturation(col, saturation);

  // Apply lightness adjustment
  col = applyLightness(col, lightness);

  return vec4(col, 1.0);
}
`,he=`
vec4 shader(vec2 fragCoord) {
  vec2 uv = fragCoord.xy / iResolution.xy;
  vec2 p[4];
  p[0] = vec2(0.1, 0.9);
  p[1] = vec2(0.9, 0.9);
  p[2] = vec2(0.5, 0.1);
  float t = iTime * timeScale;  // Use timeScale for dynamic speed
  p[3] = vec2(cos(t), sin(t)) * 0.4 + vec2(0.5, 0.5);
  vec3 c[4];
  // Add subtle color animation
  float colorShift = sin(t * 0.2) * 0.1;  // Slow color cycling
  c[0] = vec3(0.996078431372549 + colorShift, 0.3411764705882353, 0.33725490196078434);
  c[1] = vec3(0.996078431372549, 0.6352941176470588 + colorShift, 0.1607843137254902);
  c[2] = vec3(0.1450980392156863, 0.8196078431372549, 0.8588235294117647 + colorShift);
  c[3] = vec3(1.0, 1.0, 0.0);
  float blend = 2.0;
  vec3 sum = vec3(0.0);
  float valence = 0.0;
  for (int i = 0; i < 4; i++) {
      float distance = length(uv - p[i]);
      if (distance == 0.0) { distance = 1.0; }
      float w =  1.0 / pow(distance, blend);
      sum += w * c[i];
      valence += w;
  }
  sum /= valence;
  sum = pow(sum, vec3(1.0/2.2));

  // Apply hue shift to the final color
  sum = applyHueShift(sum, hueShift);

  // Apply saturation adjustment
  sum = applySaturation(sum, saturation);

  // Apply lightness adjustment
  sum = applyLightness(sum, lightness);

  return vec4(sum.xyz, 1.0);
}
`,ue=`
vec4 shader(vec2 fragCoord) {
  vec2 uv = fragCoord/iResolution.xy;
  float ratio = iResolution.x / iResolution.y;
  vec2 tuv = uv;
  tuv -= .5;
  float t = iTime * timeScale;
  float degree = noise(vec2(t * 0.1, tuv.x*tuv.y));
  tuv.y *= 1./ratio;
  tuv *= rot(radians((degree-.5)*720.+180.));
  tuv.y *= ratio;
  float frequency = 5.;
  float amplitude = 30.;
  float speed = t * 1.0;
  tuv.x += sin(tuv.y*frequency+speed)/amplitude;
  tuv.y += sin(tuv.x*frequency*1.5+speed)/(amplitude*.5);
  vec3 colorYellow = vec3(.957, .804, .623);
  vec3 colorDeepBlue = vec3(.192, .384, .933);
  vec3 layer1 = mix(colorYellow, colorDeepBlue, S(-.3, .2, (tuv*rot(radians(-5.))).x));
  vec3 colorRed = vec3(.910, .510, .8);
  vec3 colorBlue = vec3(0.350, .71, .953);
  vec3 layer2 = mix(colorRed, colorBlue, S(-.3, .2, (tuv*rot(radians(-5.))).x));
  vec3 finalComp = mix(layer1, layer2, S(.5, -.3, tuv.y));

  // Apply color adjustments
  finalComp = applyHueShift(finalComp, hueShift);
  finalComp = applySaturation(finalComp, saturation);
  finalComp = applyLightness(finalComp, lightness);

  return vec4(finalComp, 1.0);
}
`,de=`
vec4 shader(vec2 fragCoord) {
  vec2 uv = (fragCoord/iResolution.xy)*1.;
  uv.y -= 1.5;
  uv.x += .2;
  float t = iTime * timeScale;  // Use timeScale uniform
  vec2 p = uv;
  float t1 = t * 1.5;  // Reduced from 3.0 to 1.5
  float t2 = t * 0.5;  // Reduced from 1.0 to 0.5
  p.y *= (p.x*p.y) * sin(p.y*p.x + t1);  // Reduced frequency from 2. to 1.
  float d = length(p*.7);
  vec3 c0 = vec3(1.);
  vec3 c1 = vec3(.365, .794, .935);
  vec3 c2 = vec3(.973, .671, .961);
  vec3 c3 = vec3(.973, .843, .439);
  float offset = 1.2;
  float step1 = .05*offset + sin(t2*2.)*.1;  // Reduced from 3. to 2.
  float step2 = 0.3*offset + sin(t2)*.15;
  float step3 = 0.6*offset + sin(t2)*.1;
  float step4 = 1.2*offset + sin(t2*2.)*.2;  // Reduced from 3. to 2.
  vec3 col = mix(c0, c1, smoothstep(step1, step2, d));
  col = mix(col, c2, smoothstep(step2, step3, d));
  col = mix(col, c3, smoothstep(step3, step4, d));

  // Apply color adjustments
  col = applyHueShift(col, hueShift);
  col = applySaturation(col, saturation);
  col = applyLightness(col, lightness);

  return vec4(col, .5);
}
`,fe=`
vec4 shader(vec2 fragCoord) {
  vec2 uv = fragCoord/iResolution.xy;
  float ratio = iResolution.x / iResolution.y;
  vec2 tuv = uv;
  tuv -= .5;
  float t = iTime * timeScale;
  float degree = noise(vec2(t * 0.1, tuv.x*tuv.y));
  tuv.y *= 1./ratio;
  tuv *= rot(radians((degree-.5)*720.+75.));
  tuv.y *= ratio;
  float frequency = 2.;
  float amplitude = 30.;
  float speed = t * 1.0;
  tuv.x += sin(tuv.y*frequency+speed)/amplitude;
  tuv.y += sin(tuv.x*frequency*1.5+speed)/(amplitude*.5);
  vec3 colorWhite = vec3(1.0, 1.0, 1.0);
  vec3 colorRed = vec3(.914, .345, .62);
  vec3 colorPurple = vec3(.792, .573, .871);
  vec3 colorGreen = vec3(.612, .91, .364);
  vec3 colorBlue = vec3(.42, .773, .937);
  vec3 colorYellow = vec3(1.0, .973, .325);
  vec3 layer1 = mix(colorRed, colorYellow, S(-.6, .2, (tuv*rot(radians(-5.))).x));
  layer1 = mix(layer1, colorWhite, S(-.6, .2, (tuv*rot(radians(-5.))).x));
  layer1 = mix(layer1, colorPurple, S(-.2, .6, (tuv*rot(radians(-5.))).x));
  vec3 layer2 = mix(colorRed, colorYellow, S(-.8, .2, (tuv*rot(radians(-5.))).x));
  layer2 = mix(layer2, colorGreen, S(-.1, .9, (tuv*rot(radians(-5.))).x));
  layer2 = mix(layer2, colorBlue, S(-.5, .5, (tuv*rot(radians(-5.))).x));
  vec3 finalComp = mix(layer1, layer2, S(.7, -.5, tuv.y));

  // Apply color adjustments
  finalComp = applyHueShift(finalComp, hueShift);
  finalComp = applySaturation(finalComp, saturation);
  finalComp = applyLightness(finalComp, lightness);

  return vec4(finalComp, 1.0);
}
`,me=`
vec4 shader(vec2 fragCoord) {
  vec2 uv = fragCoord/iResolution.xy;
  float t = iTime * timeScale;

  // Create smooth rotation based on noise
  float degree = noise(vec2(t * 0.1, uv.x*uv.y));
  vec2 tuv = uv * 2.0 - 1.0;
  tuv *= 1.5;
  tuv *= rot(radians((degree-.5)*720.+180.));

  // Add wave distortion with adjusted scaling
  float frequency = 3.0;
  float amplitude = 40.0;
  float speed = t * 0.8;
  tuv.x += sin(tuv.y*frequency+speed)/amplitude;
  tuv.y += sin(tuv.x*frequency*1.5+speed)/(amplitude*.5);

  // Define a rich color palette
  vec3 color1 = vec3(0.957, 0.804, 0.623);
  vec3 color2 = vec3(0.192, 0.384, 0.933);
  vec3 color3 = vec3(0.910, 0.510, 0.800);
  vec3 color4 = vec3(0.350, 0.710, 0.953);

  // Create layered gradients with smooth transitions
  vec3 layer1 = mix(color1, color2, S(-.3, .2, (tuv*rot(radians(-5.))).x));
  vec3 layer2 = mix(color3, color4, S(-.3, .2, (tuv*rot(radians(-5.))).x));

  // Blend layers with smooth vertical transition
  vec3 finalColor = mix(layer1, layer2, S(.5, -.3, tuv.y));

  // Add subtle color variation based on time
  float colorShift = sin(t * 0.3) * 0.1;
  finalColor = mix(finalColor, finalColor.yzx, colorShift);

  // Add subtle vignette with wider coverage
  float vignette = smoothstep(1.0, 0.0, length(uv - 0.5));
  finalColor *= vignette;

  // Apply color adjustments
  finalColor = applyHueShift(finalColor, hueShift);
  finalColor = applySaturation(finalColor, saturation);
  finalColor = applyLightness(finalColor, lightness);

  return vec4(finalColor, 1.0);
}
`,pe=`
vec4 shader(vec2 fragCoord) {
  vec2 uv = fragCoord / iResolution.xy;
  float aspectRatio = iResolution.x / iResolution.y;
  float t = iTime * timeScale;

  // Create fluid-like movement with aspect ratio correction
  vec2 p = uv * 2.0 - 1.0;
  p.x *= aspectRatio;
  p *= 0.5; // Reduced scale for larger blobs

  // Add some noise-based distortion with lower frequency
  float noise1 = noise(p + t * 0.05); // Reduced time factor
  float noise2 = noise(p * 0.5 - t * 0.1); // Reduced scale and time factor

  // Create gradient with noise influence
  vec3 color = vec3(
    noise1 * 0.5 + 0.5,
    noise2 * 0.5 + 0.5,
    (noise1 + noise2) * 0.5
  );

  // Apply color adjustments
  color = applyHueShift(color, hueShift);
  color = applySaturation(color, saturation);
  color = applyLightness(color, lightness);

  return vec4(color, 1.0);
}
`,ge=`
vec4 shader(vec2 fragCoord) {
  vec2 uv = fragCoord / iResolution.xy;
  float aspectRatio = iResolution.x / iResolution.y;
  float t = iTime * timeScale;

  // Create multiple layers of fluid movement with aspect ratio correction
  vec2 p1 = uv * 1.0;
  p1.x *= aspectRatio;
  vec2 p2 = uv * 1.5;
  p2.x *= aspectRatio;

  // Generate noise at different scales with lower frequency
  float noise1 = noise(p1 + t * 0.05);
  float noise2 = noise(p2 - t * 0.08);
  float noise3 = noise(p1 * 0.5 + t * 0.1);

  // Combine noise layers
  float combinedNoise = (noise1 + noise2 + noise3) / 3.0;

  // Create color based on noise
  vec3 color = vec3(
    noise1 * 0.7 + 0.3,
    noise2 * 0.7 + 0.3,
    combinedNoise * 0.7 + 0.3
  );

  // Add some rotation-based variation
  vec2 rotatedUV = uv * rot(t * 0.05);
  rotatedUV.x *= aspectRatio;
  float rotationNoise = noise(rotatedUV * 0.5);
  color = mix(color, color.yzx, rotationNoise * 0.3);

  // Apply color adjustments
  color = applyHueShift(color, hueShift);
  color = applySaturation(color, saturation);
  color = applyLightness(color, lightness);

  return vec4(color, 1.0);
}
`,ve=`
vec4 shader(vec2 fragCoord) {
  vec2 uv = fragCoord / iResolution.xy;
  float aspectRatio = iResolution.x / iResolution.y;
  float t = iTime * timeScale;

  // Create abstract fluid movement with aspect ratio correction
  vec2 p = uv * 1.0;
  p.x *= aspectRatio;
  p = p * rot(t * 0.02);

  // Generate multiple noise layers with lower frequency
  float noise1 = noise(p + t * 0.05);
  float noise2 = noise(p * 0.5 - t * 0.08);
  float noise3 = noise(p * 0.25 + t * 0.1);

  // Create color channels with different noise combinations
  vec3 color = vec3(
    noise1 * noise2,
    noise2 * noise3,
    noise3 * noise1
  );

  // Add some movement-based variation
  vec2 movement = vec2(sin(t * 0.1), cos(t * 0.15)) * 0.2;
  movement.x *= aspectRatio;
  float movementNoise = noise(uv + movement);
  color = mix(color, color.zxy, movementNoise);

  // Apply color adjustments
  color = applyHueShift(color, hueShift);
  color = applySaturation(color, saturation);
  color = applyLightness(color, lightness);

  return vec4(color, 1.0);
}
`,we=`
vec3 irri(float hue) {
    return 0.5 + 0.5 * cos((9.0 * hue) + vec3(0.0, 23.0, 21.0));
}

vec2 line(vec2 p, vec2 a, vec2 b) {
    vec2 ba = b - a;
    vec2 pa = p - a;
    float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
    return vec2(length(pa - h * ba), h);
}

vec4 shader(vec2 fragCoord) {
    vec2 uv = fragCoord / iResolution.xy;
    float aspectRatio = iResolution.x / iResolution.y;
    float t = iTime * timeScale;

    // Create noise-based gradient with aspect ratio correction
    vec2 p = uv * 0.5;
    p.x *= aspectRatio;
    float noise1 = noise(p + t * 0.05);
    float noise2 = noise(p * 0.5 - t * 0.08);

    // Create gradient with noise influence
    vec3 color = vec3(
        noise1 * 0.8 + 0.2,
        noise2 * 0.8 + 0.2,
        (noise1 + noise2) * 0.4 + 0.3
    );

    // Add some rotation-based variation
    vec2 rotatedUV = uv * rot(t * 0.02);
    rotatedUV.x *= aspectRatio;
    float rotationNoise = noise(rotatedUV * 0.5);
    color = mix(color, color.yzx, rotationNoise * 0.2);

    // Apply color adjustments
    color = applyHueShift(color, hueShift);
    color = applySaturation(color, saturation);
    color = applyLightness(color, lightness);

    return vec4(color, 1.0);
}
`,ye=`
vec4 shader(vec2 fragCoord) {
    vec2 uv = fragCoord / iResolution.xy;
    float aspectRatio = iResolution.x / iResolution.y;
    float t = iTime * timeScale;

    // Create complex noise pattern with aspect ratio correction
    vec2 p1 = uv * 0.5;
    p1.x *= aspectRatio;
    vec2 p2 = uv * 0.75;
    p2.x *= aspectRatio;

    // Generate multiple noise layers with lower frequency
    float noise1 = noise(p1 + t * 0.05);
    float noise2 = noise(p2 - t * 0.08);
    float noise3 = noise(p1 * 0.25 + t * 0.1);

    // Combine noise layers with different weights
    float combinedNoise = (noise1 * 0.4 + noise2 * 0.3 + noise3 * 0.3);

    // Create color with noise influence
    vec3 color = vec3(
        noise1 * 0.6 + 0.4,
        noise2 * 0.6 + 0.4,
        combinedNoise * 0.6 + 0.4
    );

    // Add some movement-based variation
    vec2 movement = vec2(sin(t * 0.1), cos(t * 0.15)) * 0.2;
    movement.x *= aspectRatio;
    float movementNoise = noise(uv + movement);
    color = mix(color, color.zxy, movementNoise * 0.3);

    // Apply color adjustments
    color = applyHueShift(color, hueShift);
    color = applySaturation(color, saturation);
    color = applyLightness(color, lightness);

    return vec4(color, 1.0);
}
`,be={a1:ce,a2:le,b1:he,b2:ue,b3:de,b4:fe,b5:me,f1:pe,f2:ge,f3:ve,n1:we,n2:ye},xe=(i="body")=>{const e=document.querySelector(i)??document.body;return e.tagName==="CANVAS"?e:e.appendChild(Object.assign(document.createElement("canvas"),{id:"gradient-gl",style:"position:fixed;inset:0;width:100%;height:100%;z-index:-1;"}))},Se=i=>{const e=Number.parseInt(i,16);return Math.round(e*(255/15))},W=(i,e,t,o=2)=>{const r=Math.max(0,Math.min(i,15));return r===0?e:e+((r-1)/14)**o*(t-e)},_e=i=>[i.split(".").shift(),new Uint8Array(i.split(".").pop().split("").map(Se))],Ce=`#version 300 es
      in vec2 position;
      void main() {
          gl_Position = vec4(position, 0.0, 1.0);
      }`;class Ae{#e;#t;#o;#i;#d;#r;#s;#a;#n;#c;#l;constructor(e,t,o){this.#t=e,this.#c=t,this.#l=Ce,this.#d=.4,this.#r=!1,this.#a=o,this.#s=o[1],this.#n={speed:0,hueShift:0,saturation:0,lightness:0},this.#f()}#f(){this.#t.addEventListener("webglcontextlost",e=>{e.preventDefault(),this.#r=!1,this.#e=null,this.#o=null,this.#i=null,this.#t.width=0}),this.#t.addEventListener("webglcontextrestored",()=>{this.init()})}init(){this.#e=this.#m(this.#t),this.#o=this.#p(this.#l,this.#c),this.#i=this.#w(),this.#r=!0,this.#g(),this.#v(),this.#u(!0),this.#b()}#m(e){const t=e.getContext("webgl2",{antialias:!0});if(!t)throw new Error("WebGL2 not supported");return t}#h(e,t){const o=this.#e.createShader(e);this.#e.shaderSource(o,t),this.#e.compileShader(o);const r=this.#e.getShaderInfoLog(o);if(r)throw new Error(`${e===this.#e.VERTEX_SHADER?"Vertex":"Fragment"} shader compilation error: ${r}`);return o}#p(e,t){const o=this.#e.createProgram(),r=this.#h(this.#e.VERTEX_SHADER,e),n=this.#h(this.#e.FRAGMENT_SHADER,t);this.#e.attachShader(o,r),this.#e.attachShader(o,n),this.#e.linkProgram(o);const s=this.#e.getProgramInfoLog(o);return s&&console.error("Program linking error:",s),this.#e.detachShader(o,r),this.#e.detachShader(o,n),this.#e.deleteShader(r),this.#e.deleteShader(n),this.#e.useProgram(o),o}#g(){const e=this.#e.createBuffer();this.#e.bindBuffer(this.#e.ARRAY_BUFFER,e),this.#e.bufferData(this.#e.ARRAY_BUFFER,new Float32Array([-1,-1,1,-1,-1,1,1,1]),this.#e.STATIC_DRAW)}#v(){const e=this.#e.getAttribLocation(this.#o,"position");this.#e.enableVertexAttribArray(e),this.#e.vertexAttribPointer(e,2,this.#e.FLOAT,!1,0,0)}#w(){return{iResolution:this.#e.getUniformLocation(this.#o,"iResolution"),iTime:this.#e.getUniformLocation(this.#o,"iTime"),iFrame:this.#e.getUniformLocation(this.#o,"iFrame"),options:this.#e.getUniformLocation(this.#o,"options"),timeScale:this.#e.getUniformLocation(this.#o,"timeScale"),hueShift:this.#e.getUniformLocation(this.#o,"hueShift"),saturation:this.#e.getUniformLocation(this.#o,"saturation"),lightness:this.#e.getUniformLocation(this.#o,"lightness")}}#u(e=!1){if(!this.#s)return;this.#e.useProgram(this.#o),this.#e.uniform1iv(this.#i.options,this.#s);const[t,o,r,n]=this.#s.map(u=>Math.round(u*15/255)),[s,a,l,c]=[W(t,.1,3,1.5),o/15,W(r,.3,3,1.5),n/15];(e||s!==this.#n.speed||a!==this.#n.hueShift||l!==this.#n.saturation||c!==this.#n.lightness)&&(this.#e.uniform1f(this.#i.timeScale,s),this.#e.uniform1f(this.#i.hueShift,a),this.#e.uniform1f(this.#i.saturation,l),this.#e.uniform1f(this.#i.lightness,c),this.#n={speed:s,hueShift:a,saturation:l,lightness:c})}updateSeed(e){return e[0]===this.#a[0]&&e[1].every((t,o)=>t===this.#a[1][o])?!1:(this.#a=e,this.#s=e[1],this.#u(!0),!0)}#y(e){if(!this.#r||!this.#t||!this.#e)return;const{iResolution:t,iTime:o,iFrame:r}=this.#i;this.#e.useProgram(this.#o);const n=this.#t.clientWidth,s=this.#t.clientHeight;(this.#t.width!==n||this.#t.height!==s)&&(this.#t.width=n,this.#t.height=s,this.#e.uniform3f(t,this.#t.width,this.#t.height,1),this.#e.viewport(0,0,this.#t.width,this.#t.height)),this.#e.uniform1f(o,e/1e3),this.#e.uniform1f(r,Math.floor(e/1e3*60))}#b(){const e=()=>{if(!this.#r||!this.#t||!this.#e){console.log(" NOPE ");return}this.#y(performance.now()),this.#e.drawArrays(this.#e.TRIANGLE_STRIP,0,4),requestAnimationFrame(e)};e()}destroy(){if(this.#r=!1,this.#o&&this.#e&&this.#e.deleteProgram(this.#o),this.#t)try{this.#t.remove()}catch{}this.#o=null,this.#t=null,this.#e=null}}const Re=()=>Promise.resolve(ae),Ie=i=>Promise.resolve(be[i]),ke=`
  void main() {
    fragColor = shader(gl_FragCoord.xy);
  }
  `;let I=null;const Be=async(i,e="body")=>{const t=_e(i),[o]=t;if(I?.shaderId===o)return I.updateSeed(t),I;I&&(I.destroy(),I=null);const[r,n]=await Promise.all([Re(),Ie(o)]),s=r+n+ke,a=xe(e),l=new Ae(a,s,t);return l.shaderId=o,l.init(),I=l,l},Z=i=>({width:i.getAttribute("viewBox")?.split(" ")[2]||i.width.baseVal.value,height:i.getAttribute("viewBox")?.split(" ")[3]||i.height.baseVal.value}),J=({width:i,height:e})=>(i=Math.floor(i/2)*2,e=Math.floor(e/2)*2,Object.assign(document.createElement("canvas"),{width:i,height:e})),Te=(i,e)=>{const t=i.cloneNode(!0);for(const o of t.querySelectorAll("animate")){const r=o.getAttribute("values")?.split(";")||[];if(!r.length)continue;const n=r[Math.floor(e*(r.length-1))];o.setAttribute("values",n),o.setAttribute("keyTimes","0")}return t},Ee=(i,e=1,t=1)=>new Promise((o,r)=>{const n=new Blob([new XMLSerializer().serializeToString(i)],{type:"image/svg+xml"}),s=URL.createObjectURL(n),a=new Image;a.onload=()=>{URL.revokeObjectURL(s),o(a)},a.onerror=()=>{URL.revokeObjectURL(s);const l=document.createElement("canvas");l.width=e,l.height=t,o(l)},a.src=s}),$=async(i,e)=>{const t=Te(i,e),{width:o,height:r}=Z(i),n=J({width:o,height:r}),s=n.getContext("2d",{alpha:!0,willReadFrequently:!0});s.clearRect(0,0,n.width,n.height),s.fillStyle="#000",s.fillRect(0,0,n.width,n.height);const a=await Ee(t,n.width,n.height);return(a instanceof HTMLImageElement||a instanceof HTMLCanvasElement||window.ImageBitmap&&a instanceof ImageBitmap)&&s.drawImage(a,0,0,n.width,n.height),n},k={fps:30,duration:10,quality:2e6,codec:"video/webm;codecs=vp9",options:{maxBitrate:24e5,minBitrate:12e5,maxQuantizer:56,minQuantizer:20}},R=(i,...e)=>{const t=(performance.now()/1e3).toFixed(1);console.log(`[WebM ${t}s]`,i,...e)},Ue=i=>{const e=new MediaRecorder(i,{mimeType:k.codec,videoBitsPerSecond:k.quality,...k.options}),t=[];return e.ondataavailable=o=>{R(`Data chunk: ${(o.data.size/1024).toFixed(1)}KB`),t.push(o.data)},{recorder:e,chunks:t}},Le=async(i,e)=>{const t=document.getElementById("speed"),o=t?Number(t.value):5;R("User speed setting:",o);const r=o/10;R("Starting recording process",{...k,userSpeed:o,cyclesPerSecond:r.toFixed(2),cycleTimeSeconds:(1/r).toFixed(2)});const n=await $(i,0);R("First frame captured",n.width,n.height);const s=n.captureStream(k.fps);R("Stream created",s.id);const{recorder:a,chunks:l}=Ue(s);return new Promise(c=>{const h=performance.now();R("Recording started at",h),a.start(500);const u=async d=>{const m=(d-h)/1e3;if(m>=k.duration){R("Duration reached, stopping",m),a.stop();return}const p=m/k.duration;e?.(p);const f=m*r%1,w=await $(i,f),v=n.getContext("2d",{alpha:!0});v.clearRect(0,0,n.width,n.height),v.drawImage(w,0,0),requestAnimationFrame(u)};a.onstop=()=>{const d=l.reduce((p,f)=>p+f.size,0);R("Recording complete:",{chunks:l.length,totalSize:`${(d/1024/1024).toFixed(2)}MB`,avgChunkSize:`${(d/l.length/1024).toFixed(1)}KB`});const m=new Blob(l,{type:"video/webm"});c(m)},requestAnimationFrame(u)})},Me=typeof window<"u",y=65535,B=y*y,Fe=511,P=[0,20,40,60,80,99,119,139,159,179,199,219,241,264,288,313,340,367,396,427,458,491,526,562,599,637,677,718,761,805,851,898,947,997,1048,1101,1156,1212,1270,1330,1391,1453,1517,1583,1651,1720,1790,1863,1937,2013,2090,2170,2250,2333,2418,2504,2592,2681,2773,2866,2961,3058,3157,3258,3360,3464,3570,3678,3788,3900,4014,4129,4247,4366,4488,4611,4736,4864,4993,5124,5257,5392,5530,5669,5810,5953,6099,6246,6395,6547,6700,6856,7014,7174,7335,7500,7666,7834,8004,8177,8352,8528,8708,8889,9072,9258,9445,9635,9828,10022,10219,10417,10619,10822,11028,11235,11446,11658,11873,12090,12309,12530,12754,12980,13209,13440,13673,13909,14146,14387,14629,14874,15122,15371,15623,15878,16135,16394,16656,16920,17187,17456,17727,18001,18277,18556,18837,19121,19407,19696,19987,20281,20577,20876,21177,21481,21787,22096,22407,22721,23038,23357,23678,24002,24329,24658,24990,25325,25662,26001,26344,26688,27036,27386,27739,28094,28452,28813,29176,29542,29911,30282,30656,31033,31412,31794,32179,32567,32957,33350,33745,34143,34544,34948,35355,35764,36176,36591,37008,37429,37852,38278,38706,39138,39572,40009,40449,40891,41337,41785,42236,42690,43147,43606,44069,44534,45002,45473,45947,46423,46903,47385,47871,48359,48850,49344,49841,50341,50844,51349,51858,52369,52884,53401,53921,54445,54971,55500,56032,56567,57105,57646,58190,58737,59287,59840,60396,60955,61517,62082,62650,63221,63795,64372,64952,65535],G=[0,6,13,18,22,25,28,31,34,36,38,40,42,44,46,48,50,51,53,54,56,57,59,60,61,62,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,86,87,88,89,90,91,91,92,93,94,95,95,96,97,98,98,99,100,101,101,102,103,103,104,105,106,106,107,108,108,109,110,110,111,111,112,113,113,114,115,115,116,116,117,118,118,119,119,120,121,121,122,122,123,123,124,125,125,126,126,127,127,128,128,129,129,130,130,131,132,132,133,133,134,134,135,135,136,136,137,137,138,138,139,139,140,140,140,141,141,142,142,143,143,144,144,145,145,146,146,147,147,147,148,148,149,149,150,150,151,151,151,152,152,153,153,154,154,154,155,155,156,156,156,157,157,158,158,159,159,159,160,160,161,161,161,162,162,163,163,163,164,164,165,165,165,166,166,166,167,167,168,168,168,169,169,169,170,170,171,171,171,172,172,172,173,173,174,174,174,175,175,175,176,176,176,177,177,177,178,178,179,179,179,180,180,180,181,181,181,182,182,182,183,183,183,184,184,184,185,185,185,186,186,186,187,187,187,188,188,188,189,189,189,190,190,190,191,191,191,192,192,192,193,193,193,193,194,194,194,195,195,195,196,196,196,197,197,197,198,198,198,198,199,199,199,200,200,200,201,201,201,201,202,202,202,203,203,203,204,204,204,204,205,205,205,206,206,206,206,207,207,207,208,208,208,208,209,209,209,210,210,210,210,211,211,211,212,212,212,212,213,213,213,214,214,214,214,215,215,215,215,216,216,216,217,217,217,217,218,218,218,218,219,219,219,220,220,220,220,221,221,221,221,222,222,222,222,223,223,223,224,224,224,224,225,225,225,225,226,226,226,226,227,227,227,227,228,228,228,228,229,229,229,229,230,230,230,230,231,231,231,231,232,232,232,232,233,233,233,233,234,234,234,234,235,235,235,235,236,236,236,236,237,237,237,237,238,238,238,238,239,239,239,239,239,240,240,240,240,241,241,241,241,242,242,242,242,243,243,243,243,243,244,244,244,244,245,245,245,245,246,246,246,246,246,247,247,247,247,248,248,248,248,249,249,249,249,249,250,250,250,250,251,251,251,251,251,252,252,252,252,253,253,253,253,253,254,254,254,254,255,255,255];function O(i){let e;if(i<=0)return 0;if(i>=y)return y;e=i*(i*(i+-144107)/y+132114)/y+14379;for(let t=0;t<2;t++){const o=e*e*e,r=i+(2*o+B/2)/B;e=(e*(2*i+(o+B/2)/B)+r/2)/r}return e}function A(i,e){return(i^e)<0?(i-e/2)/e:(i+e/2)/e}function ee(i,e,t,o,r=!0,n=[255,255,255]){if(!r){o/=255;const s=1-o;i=i*o+s*n[0]&255,e=e*o+s*n[1]&255,t=t*o+s*n[2]&255}return{r:i,g:e,b:t}}function z(i,e,t){return i<<16|e<<8|t}function te(i,e,t){i=P[i],e=P[e],t=P[t];const o=(27015*i+35149*e+3372*t+y/2)/y,r=(13887*i+44610*e+7038*t+y/2)/y,n=(5787*i+18462*e+41286*t+y/2)/y,s=O(o),a=O(r),l=O(n);return{l:A(13792*s+52010*a-267*l,y),a:A(129628*s-159158*a+29530*l,y),b:A(1698*s+51299*a-52997*l,y)}}function q(i){if(i<=0)return 0;if(i>=y)return 255;{const e=i*Fe,t=~~(e/y),o=e%y,r=G[t],n=G[t+1];return(o*(n-r)+y/2)/y+r}}function $e(i){const e=i.l+A(25974*i.a,y)+A(14143*i.b,y),t=i.l+A(-6918*i.a,y)+A(-4185*i.b,y),o=i.l+A(-5864*i.a,y)+A(-84638*i.b,y),r=e**3/B,n=t**3/B,s=o**3/B,a=~~q((267169*r+-216771*n+15137*s+y/2)/y),l=~~q((-83127*r+171030*n+-22368*s+y/2)/y),c=~~q((-275*r+-46099*n+111909*s+y/2)/y);return{r:a,g:l,b:c}}function Ne(i){return new Promise(e=>{const t=new Image;t.decoding="sync",t.loading="eager",t.crossOrigin="anonymous",t.onload=()=>e(t),t.onerror=()=>e(t),t.src=i})}class N{constructor(){this.readable=new ReadableStream({start:e=>this._rsControler=e}),this.writable=new WritableStream({write:async e=>{let t;switch(typeof e){case"string":{const o=await Ne(e),r=N.ctx2d,n=r.canvas;r.clearRect(0,0,n.width,n.height),n.width=o.width,n.height=o.height,r.drawImage(o,0,0,n.width,n.height),t=r.getImageData(0,0,n.width,n.height).data;break}default:if(ArrayBuffer.isView(e))t=new Uint8ClampedArray(e.buffer);else if(e instanceof ArrayBuffer)t=new Uint8ClampedArray(e);else if(Array.isArray(e))if(Array.isArray(e[0])){const o=[];for(let r=e.length,n=0;n<r;n++)o.push(e[n][0]??0,e[n][1]??0,e[n][2]??0,e[n][3]??255);t=new Uint8ClampedArray(o)}else t=new Uint8ClampedArray(e);else{const o=N.ctx2d,r=o.canvas;o.clearRect(0,0,r.width,r.height),r.width=typeof e.width=="number"?e.width:e.width.baseVal.value,r.height=typeof e.height=="number"?e.height:e.height.baseVal.value,o.drawImage(e,0,0,r.width,r.height),t=o.getImageData(0,0,r.width,r.height).data}break}this._rsControler.enqueue(t)},close:()=>{this._rsControler.close()}})}static get ctx2d(){if(!this._ctx2d){if(!Me)throw new Error("Failed to get ImageToPixels.ctx2d, not in browser.");const e=document.createElement("canvas").getContext("2d",{willReadFrequently:!0});if(!e)throw new Error("Failed to get ImageToPixels.ctx2d, getContext('2d') return null.");this._ctx2d=e}return this._ctx2d}}class D{constructor(e){this.maxColors=e,this.readable=new ReadableStream({start:t=>this._rsControler=t}),this.writable=new WritableStream({write:t=>{this._rsControler.enqueue(this._boxesToQuantizedColors(this._colorsToBoxes(t)))},close:()=>{this._rsControler.close()}})}static createSorter(e){const t=e[0],o=e[1],r=e[2];return(n,s)=>n.lab[t]-s.lab[t]||n.lab[o]-s.lab[o]||n.lab[r]-s.lab[r]}_colorsToBoxes(e){let t={start:0,end:e.length-1,sorted:null,count:0,score:0,weight:0,sort:"lab",avg:{l:0,a:0,b:0}};const o=[t];let r=1;const n=(c,h,u)=>c>=h?h>=u?"lab":c>=u?"lba":"bla":c>=u?"alb":h>=u?"abl":"bal",s=c=>{const{start:h,end:u}=c;c.count=u-h+1,c.weight=0;const d={l:0,a:0,b:0};for(let p=h;p<=u;p++){const f=e[p];d.l+=f.lab.l*f.count,d.a+=f.lab.a*f.count,d.b+=f.lab.b*f.count,c.weight+=f.count}c.avg.l=d.l/c.weight,c.avg.a=d.a/c.weight,c.avg.b=d.b/c.weight;const m={l:0,a:0,b:0};for(let p=h;p<=u;p++){const f=e[p];m.l+=(f.lab.l-c.avg.l)**2*f.count,m.a+=(f.lab.a-c.avg.a)**2*f.count,m.b+=(f.lab.b-c.avg.b)**2*f.count}c.sort=n(m.l,m.a,m.b),c.score=Math.max(m.l,m.a,m.b)},a=(c,h)=>{const u={start:h+1,end:c.end,sorted:c.sorted,count:0,score:0,weight:0,sort:"lab",avg:{l:0,a:0,b:0}};s(u),c.end-=u.count,s(c),o.push(u),r++},l=()=>{let c=-1,h=-1;if(r===this.maxColors)return-1;for(let u=0;u<r;u++){const d=o[u];d.count>=2&&d.score>h&&(c=u,h=d.score)}return c};for(s(t);t&&t.count>1;){const{start:c,end:h,sort:u,sorted:d}=t;if(u!==d){const v=e.slice(c,h+1).sort(D.createSorter(u));for(let g=v.length,b=0;b<g;b++)e[c+b]=v[b];t.sorted=u}const m=t.weight+1>>1;let p=c,f=0;for(;p<h-1&&(f+=e[p].count,!(f>m));p++);a(t,p);const w=l();t=w>=0?o[w]:null}return o}_boxesToQuantizedColors(e){const t=e.reduce((o,r)=>o+r.weight,0);return e.map(o=>{const{r,g:n,b:s}=$e(o.avg);return{rgbInt:z(r,n,s),rgb:{r,g:n,b:s},hex:`#${r.toString(16).padStart(2,"0")}${n.toString(16).padStart(2,"0")}${s.toString(16).padStart(2,"0")}`,lab:o.avg,count:o.weight,percentage:o.weight/t}}).sort((o,r)=>o.rgbInt-r.rgbInt)}}class Pe{constructor(e,t,o){this.statsMode=e,this.premultipliedAlpha=t,this.tint=o,this._colors=[],this._cache=new Map,this.readable=new ReadableStream({start:r=>this._rsControler=r}),this.writable=new WritableStream({write:r=>{for(let n=r.length,s=0;s<n;s+=4){let a=r[s],l=r[s+1],c=r[s+2];const h=r[s+3];if(this.statsMode==="diff"&&this._previousPixels&&a===this._previousPixels[s]&&l===this._previousPixels[s+1]&&c===this._previousPixels[s+2]&&h===this._previousPixels[s+3])continue;({r:a,g:l,b:c}=ee(a,l,c,h,this.premultipliedAlpha,this.tint));const u=z(a,l,c),d={rgbInt:u,lab:te(a,l,c),count:1},m=u%32768;let p=this._cache.get(m);p||this._cache.set(m,p=new Map);let f=p.get(u);if(f!==void 0){this._colors[f].count++;continue}f=this._colors.push(d)-1,p.set(u,f)}this.statsMode==="diff"&&(this._previousPixels=r)},close:()=>{this._rsControler.enqueue(this._colors.slice()),this._rsControler.close(),this._colors.length=0,this._cache.clear(),this._previousPixels=void 0}})}}class oe{constructor(e=[],t=!1,o=[255,255,255]){this._premultipliedAlpha=t,this._tint=o,this._cache=new Map,this._colorMap=[],e.length&&this.setup(e)}setup(e){e=e.sort((s,a)=>s.rgbInt-a.rgbInt),this._cache.clear();const t=[],o=new Map;for(let s=-1,a=e.length,l=0;l<a;l++){const{rgbInt:c}=e[l];if(c===s){o.set(l,!0);continue}s=c}n({min:[-65535,-65535,-65535],max:[65535,65535,65535]}),this._colorMap=t;function r(s){const a={min:[65535,65535,65535],max:[-65535,-65535,-65535]},l=[];for(let p=e.length,f=0;f<p;f++){const{lab:w}=e[f];o.has(f)||w.l<s.min[0]||w.a<s.min[1]||w.b<s.min[2]||w.l>s.max[0]||w.a>s.max[1]||w.b>s.max[2]||(w.l<a.min[0]&&(a.min[0]=w.l),w.a<a.min[1]&&(a.min[1]=w.a),w.b<a.min[2]&&(a.min[2]=w.b),w.l>a.max[0]&&(a.max[0]=w.l),w.a>a.max[1]&&(a.max[1]=w.a),w.b>a.max[2]&&(a.max[2]=w.b),l.push({lab:w,index:f}))}let c="l",h=0;if(!l.length)return{index:-1,longest:c,longestIndex:h};const u=a.max[0]-a.min[0],d=a.max[1]-a.min[1],m=a.max[2]-a.min[2];return m>=u&&m>=d&&(c="b",h=2),d>=u&&d>=m&&(c="a",h=1),u>=d&&u>=m&&(c="l",h=0),{index:l.sort((p,f)=>p.lab[c]-f.lab[c])[l.length>>1].index,longest:c,longestIndex:h}}function n(s){const{index:a,longest:l,longestIndex:c}=r(s);if(a<0)return-1;o.set(a,!0);const{lab:h}=e[a],u={left:0,right:0,longest:l,lab:h,index:a},d=t.push(u)-1,m={max:[...s.max],min:[...s.min]},p={max:[...s.max],min:[...s.min]};m.max[c]=h[l],p.min[c]=Math.min(h[l]+1,65535);const f=n(m);let w=-1;return p.min[c]<=p.max[c]&&(w=n(p)),u.left=f,u.right=w,d}}_colormapNearestNode(e,t,o){const{left:r,right:n,longest:s,lab:a,index:l}=this._colorMap[e],c=Math.min((t.l-a.l)**2+(t.a-a.a)**2+(t.b-a.b)**2,4294967294);c<o.dist&&(o.index=l,o.dist=c);let h,u;if(r!==-1||n!==-1){const d=t[s]-a[s];d<=0?(h=r,u=n):(h=n,u=r),h!==-1&&this._colormapNearestNode(h,t,o),u!==-1&&d**2<o.dist&&this._colormapNearestNode(u,t,o)}}findNearestIndex(e,t,o,r=255){({r:e,g:t,b:o}=ee(e,t,o,r,this._premultipliedAlpha,this._tint));const n=z(e,t,o),s=n%32768;let a=this._cache.get(s);a||this._cache.set(s,a=new Map);let l=a.get(n);if(l!==void 0)return l;const c={dist:Number.MAX_SAFE_INTEGER,index:-1};return this._colormapNearestNode(0,te(e,t,o),c),l=c.index,a.set(n,l),l}}class Oe{constructor(e={}){this.colors=[],this.config=this._resolveOptions(e),this._stream=this._createStream()}_resolveOptions(e){const{maxColors:t=256,statsMode:o="full",algorithm:r="median-cut",premultipliedAlpha:n=!1,tint:s=[255,255,255],samples:a=[]}=e;return{maxColors:t,statsMode:o,algorithm:r,premultipliedAlpha:n,tint:s,samples:a}}_createStream(){let e;switch(this.config.algorithm){case"median-cut":default:e=new D(this.config.maxColors);break}return new ReadableStream({start:t=>{this._streamControler=t,this.config.samples.forEach(o=>t.enqueue(o))}}).pipeThrough(new N).pipeThrough(new Pe(this.config.statsMode,this.config.premultipliedAlpha,this.config.tint)).pipeThrough(e)}addSample(e){this._streamControler.enqueue(e)}generate(){return new Promise(e=>{this._streamControler.close(),this._stream.pipeTo(new WritableStream({write:t=>{this.colors=t,this.finder=new oe(t,this.config.premultipliedAlpha,this.config.tint),this._stream=this._createStream(),e(t)}}))})}match(e){var t;let o;if(typeof e=="number")o=[e>>24&255,e>>16&255,e>>8&255,e&255];else if(typeof e=="string"){const s=e.replace(/^#/,"");o=[`${s[0]}${s[1]}`,`${s[2]}${s[3]}`,`${s[4]}${s[5]}`].map(a=>parseInt(a,16))}else if(Array.isArray(e))o=e;else throw new TypeError("Unsupported color format");const r=(t=this.finder)==null?void 0:t.findNearestIndex(o[0],o[1],o[2],o[3]);if(r===void 0||r<0)return;const n=this.colors[r];if(n)return{color:n,index:r}}toColors(){return this.colors.slice()}toHexColors(){return this.colors.map(e=>e.hex)}toRgbColors(){return this.colors.map(e=>e.rgb)}toRgbIntColors(){return this.colors.map(e=>e.rgbInt)}toLabColors(){return this.colors.map(e=>e.lab)}toUint8Array(e=this.colors.length*4){var t;let o;const r=new Uint8ClampedArray(e);for(let n=0;n<e;n++){const s=n*4,a=((t=this.colors[n])==null?void 0:t.rgb)??o;a&&(r[s]=a.r,r[s+1]=a.g,r[s+2]=a.b,r[s+3]=255,o=a)}return r}clear(){this.colors.length=0,this._stream=this._createStream()}}const qe="GIF",ze=44,ie=33,De=255,He=11,Ve=249,je=4,We=59;function Ge(i){const e=new Uint8Array(i.reduce((t,o)=>t+o.byteLength,0));return i.reduce((t,o)=>(e.set(o,t),t+o.byteLength),0),e}function Ke(i,e,t){let o;if(ArrayBuffer.isView(i))o=i.buffer;else if(i instanceof ArrayBuffer)o=i;else{const r=document.createElement("canvas"),{width:n,height:s}=t||{},a=r.getContext("2d");if(!a)throw new Error("Failed to create canvas context2d");r.width=n??("width"in i?typeof i.width=="number"?i.width:i.width.baseVal.value:0),r.height=s??("height"in i?typeof i.height=="number"?i.height:i.height.baseVal.value:0),a.drawImage(i,0,0,r.width,r.height),o=a.getImageData(0,0,r.width,r.height).data.buffer}switch(e){case"uint8Array":return new Uint8Array(o);case"uint8ClampedArray":return new Uint8ClampedArray(o);case"dataView":return new DataView(o);default:throw new Error("Unsupported output format")}}function Xe(i){const e=new Image;return e.decoding="sync",e.loading="eager",e.crossOrigin="anonymous",e.src=i,e}function Ye(i){return new Promise((e,t)=>{const o=Xe(i);o.onload=()=>e(o),o.onerror=t})}function Qe(i){const e=new Map,{workerUrl:t}=i;let{workerNumber:o=1}=i;const r=[...Array.from({length:t?o:0})].map(()=>{try{const l=new Worker(t);return l.onmessage=n,l}catch(l){return console.warn(l),null}}).filter(Boolean);o=r.length;function n(l){const{id:c,data:h}=l.data;e.get(c)?.(h),e.delete(c)}const s=function(){let l=0;return c=>r[(c??l++)%o]}();return{call:function(){let l=0;return(c,h,u,d)=>new Promise(m=>{const p=s(d);if(!p)return m(void 0);e.set(l,m),p.postMessage({id:l++,type:c,data:h},{transfer:u})})}()}}class T{constructor(e=!0){this.isDebug=e}static prefix="[modern-gif]";time(e){this.isDebug&&console.time(`${T.prefix} ${e}`)}timeEnd(e){this.isDebug&&console.timeEnd(`${T.prefix} ${e}`)}debug(...e){this.isDebug&&console.debug(T.prefix,...e)}warn(...e){this.isDebug&&console.warn(T.prefix,...e)}}class Ze{constructor(e){this._config=e}_rsControler;_frames=[];readable=new ReadableStream({start:e=>this._rsControler=e});writable=new WritableStream({write:e=>{this._frames.push(e)},close:()=>{const e=this._config.backgroundColorIndex;let t;this._frames.forEach((o,r)=>{const{width:n=1,height:s=1,data:a}=o,l=o.transparent||(this._frames[r+1]?.transparent??!0);let c=0,h=0,u=n-1,d=s-1,m;if(l){for(;h<d;){let v=!0;for(let g=0;g<n;g++)if(a[n*h+g]!==e){v=!1;break}if(!v)break;h++}for(;d>h;){let v=!0;for(let g=0;g<n;g++)if(a[n*d+g]!==e){v=!1;break}if(!v)break;d--}for(;c<u;){let v=!0;for(let g=h;g<d;g++)if(a[n*g+c]!==e){v=!1;break}if(!v)break;c++}for(;u>c;){let v=!0;for(let g=h;g<d;g++)if(a[n*g+u]!==e){v=!1;break}if(!v)break;u--}}else{if(t){for(;h<d;){let v=!0;for(let g=0;g<n;g++){const b=n*h+g;if(a[b]!==t[b]){v=!1;break}}if(!v)break;h++}for(;d>h;){let v=!0;for(let g=0;g<n;g++){const b=n*d+g;if(a[b]!==t[b]){v=!1;break}}if(!v)break;d--}if(h===d)c=u;else{for(;c<u;){let v=!0;for(let g=h;g<=d;g++){const b=g*n+c;if(a[b]!==t[b]){v=!1;break}}if(!v)break;c++}for(;u>c;){let v=!0;for(let g=h;g<=d;g++){const b=g*n+u;if(a[b]!==t[b]){v=!1;break}}if(!v)break;u--}}}m=t,t=a}const p=u+1-c,f=d+1-h,w=new Uint8ClampedArray(p*f);for(let v=0;v<f;v++)for(let g=0;g<p;g++){const b=v*p+g,E=(h+v)*n+(c+g);if(!l&&m&&a[E]===m[E]){w[b]=e;continue}w[b]=a[E]}this._rsControler.enqueue({...o,left:c,top:h,width:p,height:f,disposal:l?2:1,data:w,graphicControl:{...o.graphicControl,transparent:!0,transparentIndex:e}})}),this._rsControler.close()}})}class re{constructor(e=4096){this._chunkByteLength=e,this._chunks=[this._createChunk()]}_chunks;_chunkIndex=0;_chunkOffset=0;get cursor(){return[this._chunkIndex,this._chunkOffset]}_createChunk(){return new DataView(new ArrayBuffer(this._chunkByteLength))}writeByte(e,t){t?this._chunks[t[0]].setUint8(t[1],e):(this._chunkOffset>=this._chunkByteLength&&(this._chunks[++this._chunkIndex]=this._createChunk(),this._chunkOffset=0),this._chunks[this._chunkIndex].setUint8(this._chunkOffset++,e))}writeBytes(e){e.forEach(t=>this.writeByte(t))}writeString(e){e.split("").forEach(t=>{this.writeByte(t.charCodeAt(0))})}writeUnsigned(e){this.writeBytes([e&255,e>>8&255])}calculateDistance(e){return this._chunkIndex*this._chunkByteLength+this._chunkOffset-(e[0]*this._chunkByteLength+e[1])}toUint8Array(){this._chunks[this._chunkIndex]=new DataView(this._chunks[this._chunkIndex].buffer.slice(0,this._chunkOffset));const e=new Uint8Array(this._chunks.reduce((o,r)=>o+r.byteLength,0));let t=0;return this._chunks.forEach(o=>{e.set(new Uint8Array(o.buffer),t),t+=o.byteLength}),this._chunks=[this._createChunk()],this._chunkIndex=0,this._chunkOffset=0,e}}class Je{constructor(e){this._config=e}_rsControler;_frames=[];readable=new ReadableStream({start:e=>this._rsControler=e});writable=new WritableStream({write:e=>{this._frames.push(e)},close:()=>{const e=this._encodeHeader(),t=Ge(this._frames),o=new Uint8Array(e.length+t.byteLength+1);o.set(e),o.set(t,e.byteLength),o[o.length-1]=We,this._rsControler.enqueue(o),this._rsControler.close(),this._frames.length=0}});_encodeHeader(){const e={version:"89a",looped:!0,loopCount:0,pixelAspectRatio:0,...this._config};if(e.width<=0||e.width>65535)throw new Error("Width invalid.");if(e.height<=0||e.height>65535)throw new Error("Height invalid.");let t=0;if(e.colorTable?.length){let r=e.colorTable.length;if(r<2||r>256||r&r-1)throw new Error("Invalid color table length, must be power of 2 and 2 .. 256.");for(;r>>=1;)++t;if(r=1<<t,e.colorTableSize=--t,e.backgroundColorIndex>=r)throw new Error("Background index out of range.");if(e.backgroundColorIndex===0)throw new Error("Background index explicitly passed as 0.")}const o=new re;return o.writeString(qe),o.writeString(e.version),o.writeUnsigned(e.width),o.writeUnsigned(e.height),o.writeByte(Number.parseInt(`${e.colorTableSize?1:0}1110${e.colorTableSize.toString(2).padStart(3,"0")}`,2)),o.writeByte(e.backgroundColorIndex),o.writeByte(e.pixelAspectRatio),o.writeBytes(e.colorTable?.flat()??[]),e.looped&&(o.writeByte(ie),o.writeByte(De),o.writeByte(He),o.writeString("NETSCAPE2.0"),o.writeByte(3),o.writeByte(1),o.writeUnsigned(e.loopCount),o.writeByte(0)),o.toUint8Array()}}function et(i,e,t){t.writeByte(i);let o=t.cursor;t.writeByte(0);const r=1<<i,n=r-1,s=r+1;let a=s+1,l=i+1,c=0,h=0;function u(g){for(;c>=g;)t.writeByte(h&255),h>>=8,c-=8,t.calculateDistance(o)===256&&(t.writeByte(255,o),o=t.cursor,t.writeByte(0))}function d(g){h|=g<<c,c+=l,u(8)}let m=e[0]&n,p={},f,w,v;d(r);for(let g=e.length,b=1;b<g;++b)if(v=e[b]&n,f=m<<8|v,w=p[f],w===void 0){for(h|=m<<c,c+=l;c>=8;)t.writeByte(h&255),h>>=8,c-=8,t.calculateDistance(o)===256&&(t.writeByte(255,o),o=t.cursor,t.writeByte(0));a===4096?(d(r),a=s+1,l=i+1,p={}):(a>=1<<l&&++l,p[f]=a++),m=v}else m=w;d(m),d(s),u(1),t.calculateDistance(o)===1?t.writeByte(0,o):(t.writeByte(t.calculateDistance(o)-1,o),t.writeByte(0))}class tt{constructor(e){this._config=e}_rsControler;readable=new ReadableStream({start:e=>this._rsControler=e});writable=new WritableStream({write:e=>{const t=new re,{left:o=0,top:r=0,width:n=0,height:s=0,delay:a=100,colorTable:l}=e;let{disposal:c=0}=e;const h=e.graphicControl?.transparent;let u=e.graphicControl?.transparentIndex??255;if(o<0||o>65535)throw new Error("Left invalid.");if(r<0||r>65535)throw new Error("Top invalid.");if(n<=0||n>65535)throw new Error("Width invalid.");if(s<=0||s>65535)throw new Error("Height invalid.");let d=8,m=l?l.length:0;if(m){if(m<2||m>256||m&m-1)throw new Error("Invalid color table length, must be power of 2 and 2 .. 256.");for(;m>>=1;)++d}t.writeByte(ie),t.writeByte(Ve),t.writeByte(je),h?c||(c=2):u=0,t.writeByte(Number.parseInt(`000${Number(c&7).toString(2).padStart(3,"0")}0${h?1:0}`,2)),t.writeUnsigned(a/10),t.writeByte(u),t.writeByte(0),t.writeByte(ze),t.writeUnsigned(o),t.writeUnsigned(r),t.writeUnsigned(n),t.writeUnsigned(s),l?.length?(t.writeByte(Number.parseInt(`10000${(d-1).toString(2).padStart(3,"0")}`,2)),t.writeBytes(l.flat())):t.writeByte(0),et(d,e.data,t),this._rsControler.enqueue(t.toUint8Array())},close:()=>this._rsControler.close()})}class ot{constructor(e,t){this._config=e,this._finder=new oe(t,e.premultipliedAlpha,e.tint)}_rsControler;_finder;readable=new ReadableStream({start:e=>this._rsControler=e});writable=new WritableStream({write:e=>{const t=this._config.backgroundColorIndex,o=e.data;let r=!1;const n=new Uint8ClampedArray(o.length/4);for(let s=o.length,a=0;a<s;a+=4)o[a+3]===0?(n[a/4]=t,r=!0):n[a/4]=this._finder.findNearestIndex(o[a],o[a+1],o[a+2],o[a+3]);this._rsControler.enqueue({...e,data:n,transparent:r})},close:()=>{this._rsControler.close()}})}class it{_logger;_palette;_config;_encodingFrames=[];_encodeUUID=0;_worker;constructor(e){this._logger=new T(!!e.debug),this._config=this._resolveOptions(e),this._palette=new Oe({maxColors:this._config.maxColors,premultipliedAlpha:this._config.premultipliedAlpha,tint:this._config.tint}),this._config.workerUrl?(this._worker=Qe({workerUrl:this._config.workerUrl}),this._worker.call("encoder:init",e)):this._config.frames?.forEach(t=>this.encode(t))}_resolveOptions(e){["width","height"].forEach(a=>{typeof e[a]<"u"&&Math.floor(e[a])!==e[a]&&(console.warn(`${a} cannot be a floating point number`),e[a]=Math.floor(e[a]))});const{colorTableSize:t=256,backgroundColorIndex:o=t-1,maxColors:r=t-1,premultipliedAlpha:n=!1,tint:s=[255,255,255]}=e;return{...e,colorTableSize:t,backgroundColorIndex:o,maxColors:r,premultipliedAlpha:n,tint:s}}async encode(e){if(this._worker){let s;return ArrayBuffer.isView(e.data)?s=[e.data.buffer]:e.data instanceof ArrayBuffer&&(s=[e.data]),this._worker.call("encoder:encode",e,s)}const t=this._encodeUUID;this._encodeUUID++;const{width:o=this._config.width,height:r=this._config.height}=e;let{data:n}=e;try{this._logger.time(`palette:sample-${t}`),n=typeof n=="string"?await Ye(n):n,n=Ke(n,"uint8ClampedArray",{width:o,height:r}),this._encodingFrames.push({...e,width:o,height:r,data:n}),this._palette.addSample(n)}finally{this._logger.timeEnd(`palette:sample-${t}`)}}async flush(e){if(this._worker)return this._worker.call("encoder:flush",e);this._logger.time("palette:generate");const t=await this._palette.generate();this._logger.timeEnd("palette:generate");const o=t.map(n=>[n.rgb.r,n.rgb.g,n.rgb.b]);for(;o.length<this._config.colorTableSize;)o.push([0,0,0]);this._logger.debug("palette:maxColors",this._config.maxColors),this._logger.isDebug&&console.debug(t.map(()=>"%c ").join(""),...t.map(n=>`margin: 1px; background: ${n.hex}`)),this._logger.time("encode");const r=await new Promise(n=>{new ReadableStream({start:s=>{this._encodingFrames.forEach(a=>{s.enqueue(a)}),s.close()}}).pipeThrough(new ot(this._config,t)).pipeThrough(new Ze(this._config)).pipeThrough(new tt(this._config)).pipeThrough(new Je({...this._config,colorTable:o})).pipeTo(new WritableStream({write:s=>n(s)}))});switch(this._logger.timeEnd("encode"),this._encodingFrames=[],this._encodeUUID=0,e){case"blob":return new Blob([r.buffer],{type:"image/gif"});case"arrayBuffer":default:return r.buffer}}}function rt(i){return new it(i).flush(i.format)}const K=30,X=10,nt=async(i,e)=>{const o=(Number(document.getElementById("speed")?.value)||5)/10,r=K*X,{width:n,height:s}=Z(i),a=Math.floor(n),l=Math.floor(s),c=1e3/K,h=Array.from({length:r},(p,f)=>f),u=[];await h.reduce((p,f)=>p.then(async()=>{const v=f/r*X*o%1;e?.(f/r);const g=await $(i,v);u.push(g)}),Promise.resolve());const d=u.map(p=>({data:p,delay:c})),m=await rt({width:a,height:l,frames:d,maxColors:255,premultipliedAlpha:!1,debug:!1});return new Blob([m],{type:"image/gif"})},st=i=>URL.createObjectURL(i),at=async(i,e)=>{e(1);const t=await $(i,Math.random());return new Promise(o=>{t.toBlob(r=>{o(r)},"image/png")})},ct=(i,e)=>{const t=document.createElement("a");t.href=i,t.download=`glitch_${~~performance.now()}.${e}`,t.click(),URL.revokeObjectURL(i)},Y={percentage:i=>`${(i*100).toFixed()}%`,timer:i=>`${i.toFixed(1)}s`},lt=i=>({percentage:()=>({update:t=>{i.textContent=Y.percentage(t)},cleanup:()=>{}}),timer:()=>{let e=0;const t=setInterval(()=>{e+=.1,i.textContent=Y.timer(e)},100);return{update:()=>{},cleanup:()=>clearInterval(t)}}}),ht=async(i,e,t="percentage")=>{const o=i.textContent;i.disabled=!0;const r=lt(i)[t]();try{return await e(r.update)}catch(n){return console.error(`${i.id} capture failed:`,n),null}finally{r.cleanup(),i.disabled=!1,i.textContent=o,console.log("blur"),i.blur()}},ut=(i,e)=>(e(1),Promise.resolve(new Blob([new XMLSerializer().serializeToString(i)],{type:"image/svg+xml;charset=utf-8"}))),M=(i,e,t="percentage")=>async(o,r)=>{const n=await ht(r,s=>i(o,s),t);n&&ct(st(n),e)},dt={svg:M(ut,"svg"),png:M(at,"png"),webm:M(Le,"webm"),gif:M(nt,"gif","timer")},Q={red:"1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0",green:"0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0",blue:"0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0"},ne=(i=8)=>[0,...Array.from({length:i-2},()=>(Math.random()*.92+.08).toFixed(4)).sort((e,t)=>e-t),1].join(";"),se=(i,e=8)=>Array.from({length:e},()=>(Math.random()*(i-.001)+.001).toFixed(4)).join(";"),ft=(i,e,t,o,r)=>{const n=se(r),s=ne();return`
       <feOffset in="blended" dx="0" dy="0" y="${e}%" height="${t}%" result="slice_${i}">
         <animate attributeName="dx" keyTimes="${s}" values="${n}" 
                  begin="0s" dur="${o}s" calcMode="discrete" repeatCount="indefinite" fill="freeze" />
         <animate attributeName="dy" keyTimes="${s}" values="${n}" 
                  begin="0s" dur="${o}s" calcMode="discrete" repeatCount="indefinite" fill="freeze" />
       </feOffset>
  `},mt=i=>{const e=Object.assign(document.createElement("span"),{style:`
      font-family: monospace, serif;
      font-weight: bolder;
      font-size: 24px;
      visibility: hidden;
      position: absolute;
      white-space: nowrap;
      letter-spacing: 0.1em;
    `,textContent:i});document.body.appendChild(e);const t=e.getBoundingClientRect().width;return document.body.removeChild(e),t},pt=i=>{const e=mt(i),t=e*.02,o=e+t*2,r=2.5;return{width:Math.max(50,o)*r,height:48*r,fontSize:26*r}},gt=(i,e)=>{const t=i<3?300-i*80:i<7?60*.3**(i-3):20*.2**(i-7);return e.map(o=>{const r=(1+o/100)*(.8+Math.random()*.4);return Math.max(.05,(t*r).toFixed(4))})},vt=(i,e)=>{const t=!!i.img,o=i.text,r=i.img,{speed:n,intensity:s,colorSep:a,heightVariation:l}=e,c=t?{width:i.imgWidth||420,height:i.imgHeight||120,fontSize:0}:pt(o),{width:h,height:u,fontSize:d}=c,m=Array.from({length:8},()=>Math.floor(Math.random()*(6+l*2-6)+6)),p=m.reduce((x,S)=>x+S,0),f=m.map(x=>Math.max(6,Math.round(x/p*100))),w=f.reduce((x,S)=>x+S,0);f[f.length-1]+=100-w;const v=f.map((x,S)=>f.slice(0,S).reduce((U,L)=>U+L,0)),g=gt(n,f),b=f.map((x,S)=>ft(S,v[S],x,g[S],s)),E=x=>`      <feColorMatrix 
  in="SourceGraphic" result="${x}" type="matrix" values="${Q[x]}" /> `,V=(x,S,U)=>{const L=ne(),j=se(a);return`
      <feOffset in="${x}" result="${x}-shifted" dx="${S}" dy="0">
        <animate attributeName="dx" keyTimes="${L}" values="${j}" 
                 begin="0" dur="${U}s" calcMode="discrete" repeatCount="indefinite" fill="freeze" />
        <animate attributeName="dy" keyTimes="${L}" values="${j}" 
               begin="0" dur="${U}s" calcMode="discrete" repeatCount="indefinite" fill="freeze" />
      </feOffset>
    `};return`<svg xmlns="http://www.w3.org/2000/svg" 
            width="${Math.round(h)}px" 
            height="${u}px" 
            viewBox="0 0 ${Math.round(h)} ${u}">
  <!-- made with glitcher-app v0.7.0 -->
  <!-- https://github.com/metaory/glitcher-app -->
  <!-- MIT License (c) 2025 metaory -->
  ${t?`<image href="${r}" x="0" y="0" width="100%" height="100%" filter="url(#glitch)" style="image-rendering:pixelated;" />`:`<text
    filter="url(#glitch)" 
    fill="#FFFFFF" 
    font-family="monospace, sans-serif"
    font-weight="bolder" 
    font-size="${d}" 
    text-anchor="middle" 
    dominant-baseline="middle" 
    stroke="#FFFFFF"
    stroke-width="2"
    x="48%" 
    y="50%"
  >${o}</text>`}
  <defs>
    <filter id="glitch" primitiveUnits="objectBoundingBox" x="-10%" y="0%" width="120%" height="100%">
${Object.keys(Q).map(E).join(`
`)}
       ${V("red",-.01,n)}
       ${V("blue",.01,n*1.1)}
       <feBlend mode="screen" in="red-shifted" in2="green" result="red-green" />
       <feBlend mode="screen" in="red-green" in2="blue-shifted" result="blended" />
       ${b.join("")}
      <feMerge>
        ${b.map((x,S)=>`<feMergeNode in="slice_${S}" />`).join("")}
      </feMerge>
    </filter>
  </defs>
</svg>
  `.replace(/\n\s*\n/g,`
`)};Be("a2.af4a");const _=document.getElementById.bind(document),wt=document.querySelector.bind(document),F=Number.parseFloat,yt=i=>{const e=i.target.previousElementSibling?.querySelector("span");e&&(e.textContent=i.target.value)},bt=()=>_("textInput").value||"ḠĿ𐪌𐌕ꛕⵤM꠵",xt=()=>({speed:F(_("speed").value),intensity:F(_("intensity").value),colorSep:F(_("colorSep").value),heightVariation:F(_("slices").value)}),C=new Proxy({img:null},{set(i,e,t){return i[e]=t,H(),!0}}),St=(i,e=512)=>new Promise(t=>{const o=new window.Image;o.onload=()=>{const r=Math.min(1,e/Math.max(o.width,o.height)),n=Math.round(o.width*r),s=Math.round(o.height*r),a=J({width:n,height:s});a.getContext("2d").drawImage(o,0,0,n,s),t({dataUrl:a.toDataURL("image/png"),width:n,height:s})},o.src=URL.createObjectURL(i)});_("imgInput").onchange=i=>{const e=i.target.files[0];e?St(e,512).then(({dataUrl:t,width:o,height:r})=>{C.img=t,C.imgWidth=o,C.imgHeight=r}):(C.img=null,C.imgWidth=null,C.imgHeight=null),_("textInput").value=""};_("textInput").addEventListener("input",i=>{C.img&&(C.img=null)});const H=i=>{i?.target?.type==="range"&&(yt(i),_("textInput").blur());const e=!!C.img;_("preview").innerHTML=vt({text:e?"":bt(),img:C.img||null,imgWidth:e?C.imgWidth:void 0,imgHeight:e?C.imgHeight:void 0},xt())};for(const i of["textInput","speed","intensity","colorSep","slices"])_(i).addEventListener("input",H);const _t=i=>{const e=_(`download-${i}`);e.onclick=()=>dt[i](wt("#preview svg"),e)};["webm","svg","png","gif"].forEach(_t);_("version").textContent="v1.1.0";H();
//# sourceMappingURL=index-ByoCjdh8.js.map
