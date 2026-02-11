# Python port (WIP)

Fortran 기반 전처리기의 **초기 단계만** Python으로 옮겨둔 작업본입니다.

## 현재 구현된 범위
- INI/path 로딩 (`init` 대응)
- `spref_etc.dat` 읽기 (`read_spref` / `init_spref` 대응)
- PDMS 임시 입력(`temp.dat`) 읽기 (`import_csv_PDMS` 대응)
- PDMS row 정리 (`trim_csv_PDMS` 대응)
- 트리 구성/정렬/브랜치 계산의 Python 버전
  - `arrange_PDMS` 대응
  - `trim_twork_iChilds` 대응(핵심 규칙만)
  - `sort_tree` 대응
  - `make_bwork` 대응

## 아직 미구현
- `trim_PDMS_info2` 상세 보정
- `make_dsheet` / `export_dsheet` 완전 호환
- `export_tree`, `export_tree_expand` 파일 포맷 1:1 재현

---

## 실행 절차 (처음 하는 기준)

네, 로컬에 내려받아서 실행하면 됩니다.
아래 순서대로 하시면 됩니다.

### 1) Python 버전 확인
```bash
python --version
```
- Python 3.10+ 권장

### 2) 프로젝트 폴더로 이동
```bash
cd /path/to/temp1
```

### 3) (선택) 가상환경 생성
```bash
python -m venv .venv
source .venv/bin/activate
```

### 4) 입력 파일 준비
CLI는 `--ini`로 넘긴 ini 파일에서 아래 경로를 읽습니다.
- `[LOCALTEMPPATH]`
- `[SERVERPATH]`
- `[LIBRARYPATH]`

그리고 실제로는 아래 파일들을 사용합니다.
- `<LOCALTEMPPATH>/tmp/temp.dat`
- `<LOCALTEMPPATH>/spref_etc.dat` (`[LIBRARYPATH]` 값이 있을 때)

### 5) 실행
```bash
python -m python_port.cli --ini /path/to/hhiducta.ini
python -m python_port.cli --ini /path/to/hhiducta.ini --tree
```

### 6) 결과 확인
현재 버전은 최종 `temp_dsheet.dexp`까지는 아직 미완성이며,
아래처럼 요약 정보를 출력합니다.
- INI 경로
- LocalTempPath
- SPREF ETC rows
- PDMS rows/cols
- (`--tree` 사용 시) 노드/루트/브랜치 통계

---

## 빠른 점검 명령
```bash
python -m python_port.cli --help
python -m pytest -q
```
