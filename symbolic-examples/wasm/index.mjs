import { WASI } from "https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";
import { blake2b } from "./blake2b.js";
import fs from "node:fs";

async function initialiseWASI() {
    const wasi = new WASI({
        stdout: (out) => console.log("[wasm stdout]", out),
        blake2b: blake2b
    });
    
    const jsffiExports = {};
    const src = fs.readFileSync('./proof.wasm');
    const wasm_bin = await WebAssembly.instantiate(
        src,
        Object.assign(
            { ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports), blake2b: blake2b },
            wasi.getImportObject()
        )
    );
    Object.assign(jsffiExports, wasm_bin.instance.exports);
    
    wasi.initialize(wasm_bin, {
        ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports),
        blake2b: blake2b
    });
    
    return wasi.instance; 
}

function mkProofBytesMock(instance, x, ps, empi) {
    const xStr = x.toString() + "\0";
    const psStr = ps.map((x) => x.toString()).join(" ") + "\0";
    const empiStr = [empi.e.toString(), empi.n.toString(), empi.sig.toString(), empi.tokenName.toString()].join(" ") + "\0";

    const xOffset = 0;
    const psOffset = xStr.length;
    const empiOffset = psOffset + psStr.length;

    let buffer = instance.exports.memory.buffer;

    const xBuf = new Uint8Array(buffer, xOffset, xStr.length).fill();
    const psBuf = new Uint8Array(buffer, psOffset, psStr.length).fill();
    const empiBuf = new Uint8Array(buffer, empiOffset, empiStr.length).fill();
    
    const utf8Encode = new TextEncoder();

    const xBytes = utf8Encode.encode(xStr);
    const psBytes = utf8Encode.encode(psStr);
    const empiBytes = utf8Encode.encode(empiStr);

    xBuf.forEach((v,i,a) => a[i] = xBytes[i]);
    psBuf.forEach((v,i,a) => a[i] = psBytes[i]);
    empiBuf.forEach((v,i,a) => a[i] = empiBytes[i]);

    const address = instance.exports.mkProofBytesMockWasm(xBuf.byteOffset, psBuf.byteOffset, empiBuf.byteOffset);

    buffer = instance.exports.memory.buffer; // Refresh the reference. The old buffer becomes "detached" when it grows.

    const encodedStringLength = (new Uint8Array(buffer, address)).indexOf(0);
    const encodedStringBuffer = new Uint8Array(buffer, address, encodedStringLength);
    const result = (new TextDecoder()).decode(encodedStringBuffer);
    console.log(result);
    const json = JSON.parse(result);
    return json;
}

const x = 123n;
const ps = new Array(19).fill(0n);
const empi = {
    e: 10n,
    n: 100n,
    sig: 11n,
    tokenName: 123n
};

const instance = await initialiseWASI();
mkProofBytesMock(instance, x, ps, empi);
