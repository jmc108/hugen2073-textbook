import os
import pathlib
import requests
from openai import OpenAI

# -------- Required env vars --------
OPENAI_API_KEY = os.environ["OPENAI_API_KEY"]
CLOUDFLARE_API_TOKEN = os.environ["CLOUDFLARE_API_TOKEN"]
CLOUDFLARE_ACCOUNT_ID = os.environ["CLOUDFLARE_ACCOUNT_ID"]
CLOUDFLARE_KV_NAMESPACE_ID = os.environ["CLOUDFLARE_KV_NAMESPACE_ID"]

LECTURE_ROOT = pathlib.Path("lecture_slides")
VECTOR_STORE_PREFIX = "hugen2071-"  # stable naming for per-chapter vector stores

client = OpenAI(api_key=OPENAI_API_KEY)

CF_HEADERS = {
    "Authorization": f"Bearer {CLOUDFLARE_API_TOKEN}",
    # NOTE: KV PUT uses raw body; content-type not required but harmless
}

def kv_put(key: str, value: str) -> None:
    url = (
        f"https://api.cloudflare.com/client/v4/accounts/{CLOUDFLARE_ACCOUNT_ID}"
        f"/storage/kv/namespaces/{CLOUDFLARE_KV_NAMESPACE_ID}"
        f"/values/{key}"
    )
    r = requests.put(url, headers=CF_HEADERS, data=value.encode("utf-8"))
    r.raise_for_status()

def get_or_create_vector_store(chapter_id: str) -> str:
    target_name = f"{VECTOR_STORE_PREFIX}{chapter_id}"

    # list() is paginated in some clients; for small numbers this is fine.
    stores = client.vector_stores.list().data
    for vs in stores:
        if vs.name == target_name:
            return vs.id

    vs = client.vector_stores.create(name=target_name)
    return vs.id

def clear_vector_store_files(vector_store_id: str) -> None:
    files = client.vector_stores.files.list(vector_store_id).data
    for f in files:
        client.vector_stores.files.delete(
            vector_store_id=vector_store_id,
            file_id=f.id,
        )



def attach_pdfs(vector_store_id: str, pdf_paths: list[pathlib.Path]) -> None:
    for pdf in pdf_paths:
        uploaded = client.files.create(file=open(pdf, "rb"), purpose="assistants")
        client.vector_stores.files.create(vector_store_id, file_id=uploaded.id)

def read_text_if_exists(path: pathlib.Path) -> str:
    if path.exists():
        return path.read_text(encoding="utf-8")
    return ""

def sync_one_chapter(chapter_dir: pathlib.Path) -> None:
    chapter_id = chapter_dir.name
    print(f"\n=== Syncing {chapter_id} ===")

    pdfs = sorted(chapter_dir.glob("*.pdf"))
    if not pdfs:
        print(f"Skipping {chapter_id}: no PDFs found in {chapter_dir}")
        return


    prompt_txt = read_text_if_exists(chapter_dir / "prompt.txt")
    checks_md = read_text_if_exists(chapter_dir / "checks.md")

    vs_id = get_or_create_vector_store(chapter_id)
    print(f"Vector store: {vs_id}")

    # Replace contents (simple + reliable)
    clear_vector_store_files(vs_id)
    attach_pdfs(vs_id, pdfs)
    print(f"Attached PDFs: {[p.name for p in pdfs]}")

    # Update KV so Worker can find everything
    kv_put(f"chapter:{chapter_id}:vs_id", vs_id)
    kv_put(f"chapter:{chapter_id}:prompt", prompt_txt)
    kv_put(f"chapter:{chapter_id}:checks", checks_md)

    print("KV updated.")

def main():
    if not LECTURE_ROOT.exists():
        raise RuntimeError("lecture_slides/ not found (run from repo root).")

    chapter_dirs = [p for p in LECTURE_ROOT.iterdir() if p.is_dir()]
    if not chapter_dirs:
        raise RuntimeError("No chapter folders found under lecture_slides/")

    for d in sorted(chapter_dirs, key=lambda p: p.name):
        sync_one_chapter(d)

if __name__ == "__main__":
    main()
