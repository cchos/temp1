# Python port (WIP)

Fortran 기반 전처리기의 Python 포팅입니다.

## 현재 구현된 범위
- INI/path 로딩 (`init` 대응)
- `spref_etc.dat` 읽기 (`read_spref` / `init_spref` 대응)
- PDMS 임시 입력(`temp.dat`) 읽기 (`import_csv_PDMS` 대응)
- PDMS row 정리 (`trim_csv_PDMS` 대응)
- 트리 구성/정렬/브랜치 계산의 Python 버전
  - `arrange_PDMS` 대응
  - `trim_twork_iChilds` 대응(핵심 규칙)
  - `sort_tree` 대응
  - `make_bwork` 대응
- 후처리/출력
  - `trim_PDMS_info2`의 단위 문자 제거(부분)
  - `temp_dsheet.dexp` 내보내기 (`export_dsheet` 대응, 근사 매핑)

## 남은 작업 (완전 1:1 위해)
- `trim_PDMS_info2`의 SPREF 매칭/보정 로직 전체
- `make_dsheet` 세부 계산식 완전 일치
- `export_tree`, `export_tree_expand` 포맷까지 완전 일치

---

## 실행 절차

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

### 4) 실행
```bash
# 초기 단계만
python -m python_port.cli --ini /path/to/hhiducta.ini

# 트리/브랜치 단계까지
python -m python_port.cli --ini /path/to/hhiducta.ini --tree

# dsheet 파일 출력까지
python -m python_port.cli --ini /path/to/hhiducta.ini --full
python -m python_port.cli --ini /path/to/hhiducta.ini --full --out /path/to/temp_dsheet.dexp
```

### 5) 빠른 점검
```bash
python -m python_port.cli --help
python -m pytest -q
```
