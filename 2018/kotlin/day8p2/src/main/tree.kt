package main

import java.util.*

data class Node(val metadata: List<Int>, val children: List<Node>) {
    fun metadataSum(): Int {
        return metadata.sum() + children.map { it.metadataSum() }.sum()
    }

    fun value(): Int {
        val values =  if (children.count() == 0) {
            metadata
        } else {
            metadata.map { n -> if (n <= children.count() ) children[n-1].value() else 0 }
        }
        return values.sum()
    }

    companion object {
        fun parse(input: String): Node {
            val numbers = ArrayDeque(input.split(' ').map { it.toInt() })
            val root = build(numbers)

            if (!numbers.isEmpty()) {
                throw Error("Input was not exhausted")
            }

            return root;
        }

        private fun build(numbers: Queue<Int>): Node {
            val nChildren = requireNext(numbers)
            val nMetas = requireNext(numbers)
            val children = (1..nChildren).map { build(numbers) }
            val metas = (1..nMetas).map { requireNext(numbers) }
            return Node(metas, children)
        }

        private fun requireNext(numbers: Queue<Int>): Int {
            return numbers.poll() ?: throw Error("Unexpected end of input")
        }
    }
}