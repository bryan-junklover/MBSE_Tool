#!/usr/bin/env python3
"""A stand-in for LM Studio, for browser checks without a real model.
Answers any chat request with a fixed coach or gap answer. Usage: mock_lmstudio.py [port] [delay_s]"""
import json, sys, time
from http.server import BaseHTTPRequestHandler, HTTPServer

DELAY = float(sys.argv[2]) if len(sys.argv) > 2 else 0.2
COACH = {"criteria": {"singular": "pass", "verifiable": "pass", "design_independent": "question", "conflicts": "none", "fits_scope": "pass"},
         "questions": [{"text": "What happens if the power system can't reach the flight controller?", "context": "Mock answer."}],
         "example_rewrite": "When the bus drops, the power system shall report its status within 9 s.", "conflicts": []}
GAPS = {"items": [
    {"kind": "question", "title": "What keeps the battery inside its temperature range?", "body": "Mock answer.", "target": "", "confidence": "medium", "name": "", "stereotype": "", "draft": "", "ports": []},
    {"kind": "sketch_element", "title": "What notices if the Flight Controller stops?", "body": "Mock answer.", "target": "", "confidence": "medium", "name": "Watchdog 5000", "stereotype": "HW", "draft": "", "ports": []},
]}

class Handler(BaseHTTPRequestHandler):
    def log_message(self, *args): pass
    def send_json(self, value):
        body = json.dumps(value).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)
    def do_GET(self):
        self.send_json({"data": [{"id": "mock-model"}]})
    def do_POST(self):
        request = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        time.sleep(DELAY)
        text = json.dumps(request.get("messages", ""))
        answer = GAPS if "what a system model is missing" in text else COACH
        self.send_json({"id": "x", "object": "chat.completion", "model": "mock-model", "choices": [{"index": 0, "finish_reason": "stop",
                        "message": {"role": "assistant", "content": json.dumps(answer)}}], "usage": {"prompt_tokens": 10, "completion_tokens": 5, "total_tokens": 15}})

HTTPServer(("127.0.0.1", int(sys.argv[1]) if len(sys.argv) > 1 else 1234), Handler).serve_forever()
