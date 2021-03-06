/*
 * This file is part of Apparat.
 *
 * Copyright (C) 2010 Joa Ebert
 * http://www.joa-ebert.com/
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */
package apparat.graph

import analysis.{StronglyConnectedComponentFinder, Dominance}
import apparat.utils.{IndentingPrintWriter, Dumpable}

/**
 * @author Joa Ebert
 */
trait GraphLike[V] extends Dumpable {
	type G <: GraphLike[V]
	type E = Edge[V]

	def topsort: GraphTraversal[V]

	def dominance: Dominance[V]

	def sccs: StronglyConnectedComponentFinder[V]

	def contains(vertex: V): Boolean

	def contains(edge: E): Boolean

	def outgoingOf(vertex: V): Iterable[E]

	def incomingOf(vertex: V): Iterable[E]

	def predecessorsOf(vertex: V): Iterable[V]

	def successorsOf(vertex: V): Iterable[V]

	def outdegreeOf(vertex: V): Int

	def indegreeOf(vertex: V): Int

	def verticesIterator: Iterator[V]

	def edgesIterator: Iterator[E]

	def dft(vertex: V): GraphTraversal[V] = new DepthFirstTraversal(this, vertex)

	def vertexExists(p: V => Boolean) = verticesIterator exists p

	def edgeExists(p: E => Boolean) = edgesIterator exists p

	def foreachVertex(f: V => Unit) = verticesIterator foreach f

	def foreachEdge(f: E => Unit) = edgesIterator foreach f

	def vertexMap[T](f: V => T): Map[V, T] = Map(verticesIterator map {v => v -> f(v)} toSeq: _*)

	def edgeMap[T](f: E => T): Map[E, T] = Map(edgesIterator map {e => e -> f(e)} toSeq: _*)

	def -(edge: E): G

	def +(edge: E): G

	def -(vertex: V): G

	def +(vertex: V): G

	def replace(v0: V, v1: V): G

	def optimized: G

	override def dump(writer: IndentingPrintWriter) = {
		writer <= "Graph:"
		writer withIndent {
			for (vertex <- verticesIterator) {
				writer <= vertex.toString
				writer withIndent {
					writer.println(outgoingOf(vertex)) {
						edge => (if (edge.kind != EdgeKind.Default) edge.kind.toString else "") + " -> " + edge.endVertex.toString
					}
				}
			}
		}
	}
}
