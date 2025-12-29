# #!/bin/bash

# # 출력 제어
# QUIET=true

# log_step() {
#     if [ "$QUIET" = false ]; then
#         echo "$1"
#     fi
# }

# log_error() {
#     echo "❌ $1" >&2
# }

# log_success() {
#     echo "✅ $1"
# }

# # 1. LSP 서버 빌드
# log_step "[1/3] LSP 서버 빌드..."
# cd ../src/FSLint.LanguageServer
# if ! dotnet publish -c Release -o ../../vscode-extension/bin > /dev/null 2>&1; then
#     log_error "LSP 서버 빌드 실패"
#     exit 1
# fi

# # 2. Extension 빌드
# log_step "[2/3] Extension 빌드..."
# cd ../../vscode-extension
# if ! npm install --silent > /dev/null 2>&1; then
#     log_error "npm install 실패"
#     exit 1
# fi

# if ! npm run compile --silent > /dev/null 2>&1; then
#     log_error "Extension 빌드 실패"
#     exit 1
# fi

# # 3. Extension 패키징
# log_step "[3/3] Extension 패키징..."
# if ! echo -e "y\ny\ny" | npx vsce package --no-dependencies > /dev/null 2>&1; then
#     log_error "패키징 실패"
#     exit 1
# fi

# # 4. Extension 설치
# VSIX_FILE=$(ls -t *.vsix | head -1)

# if [ -z "$VSIX_FILE" ]; then
#     log_error "VSIX 파일을 찾을 수 없습니다"
#     exit 1
# fi

# code --install-extension "$VSIX_FILE" --force 2>&1 | grep "successfully installed"


echo "======================================"
echo "FSLint Extension 빌드 및 설치"
echo "======================================"

# 1. LSP 서버 빌드
echo "[1/3] LSP 서버 빌드..."
cd ../src/FSLint.LanguageServer
dotnet publish -c Release -o ../../vscode-extension/bin
if [ $? -ne 0 ]; then
    echo "❌ LSP 서버 빌드 실패"
    exit 1
fi

# 2. Extension 빌드
echo "[2/3] Extension 빌드..."
cd ../../vscode-extension
npm install
npm run compile
if [ $? -ne 0 ]; then
    echo "❌ Extension 빌드 실패"
    exit 1
fi

# 3. Extension 패키징
echo "[3/3] Extension 패키징..."
npx vsce package
if [ $? -ne 0 ]; then
    echo "❌ 패키징 실패"
    exit 1
fi

# 4. Extension 설치
VSIX_FILE=$(ls -t *.vsix | head -1)
echo ""
echo "✅ 빌드 완료: $VSIX_FILE"
echo ""
echo "설치 명령:"
echo "  code --install-extension $VSIX_FILE"
echo ""
read -p "지금 설치하시겠습니까? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
    code --install-extension $VSIX_FILE
    echo "✅ 설치 완료! VS Code를 재시작하세요."
fi