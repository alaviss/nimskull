import openssl
import std/strutils

echo "Version: 0x", toHex(OpenSSL_version_num(), 64)
