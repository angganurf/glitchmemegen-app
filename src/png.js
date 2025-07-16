import { captureState, getSVGDimensions } from './svgcap.js';

export const exportSingleFramePNG = async (svg, time = 0.5, filename = 'glitched.png') => {
    const { width, height } = getSVGDimensions(svg);
    const canvas = await captureState(svg, time); // ambil snapshot glitch di waktu tertentu

    // Buat DataURL dari canvas
    const dataURL = canvas.toDataURL('image/png');

    // Buat dan trigger <a> untuk download
    const a = document.createElement('a');
    a.href = dataURL;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
};
