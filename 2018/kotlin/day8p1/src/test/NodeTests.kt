package test

import org.junit.jupiter.api.Test
import main.Node
import kotlin.test.assertEquals

class NodeTests {
    @Test
    fun testParse() {
        val input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2";
        val expected = Node(
                metadata = listOf(1, 1, 2),
                children = listOf(
                        Node(metadata = listOf(10, 11, 12), children = emptyList()),
                        Node(
                                metadata = listOf(2),
                                children = listOf(
                                        Node(metadata = listOf(99), children = emptyList())
                                )
                        )
                )
        )
        assertEquals(expected, Node.parse(input))
    }

    @Test
    fun testMetadataSum() {
        val root = Node(
                metadata = listOf(1, 1, 2),
                children = listOf(
                        Node(metadata = listOf(10, 11, 12), children = emptyList()),
                        Node(
                                metadata = listOf(2),
                                children = listOf(
                                        Node(metadata = listOf(99), children = emptyList())
                                )
                        )
                )
        )
        assertEquals(138, root.metadataSum())
    }
}